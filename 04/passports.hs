#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [interpolatedstring-perl6 lens text validation])"

{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

import Control.Lens

import Control.Monad (msum, unless)
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Validation (Validation,_Failure, _Success)
import qualified Data.Validation as Val
import Text.InterpolatedString.Perl6 (qq)
import Text.Read (readMaybe)

type PassportData = Map Text Text

-- Read and parse standard input
parse :: IO (Seq PassportData)
parse =
  go [] [] . fmap (fmap keyVal) . fmap Text.words . Text.lines . decodeUtf8 <$> B.getContents
  where
    keyVal txt = let (key, val0) = Text.breakOn ":" txt in (key, Text.drop 1 val0)
    go rs [] [] = rs
    go rs fs [] = go (rs Seq.|> Map.fromList fs) [] []
    go rs fs ([]:xs) = go (rs Seq.|> Map.fromList fs) [] xs
    go rs fs (x:xs) = go rs (fs++x) xs

data PassportError
  = MissingRequiredField Text
  | FieldParseError Text Text
  | BadByr Int Int Int
  | BadIyr Int Int Int
  | BadEyr Int Int Int
  | MissingHgtSuffix Text
  | BadHgt Int Text Int Int
  | BadHcl Text
  | BadEcl Text
  | BadPid Text
  deriving (Eq, Ord, Show)

prettyPassportError :: PassportError -> Text
prettyPassportError = \case
  MissingRequiredField txt ->
    [qq|Missing required field {txt}|]
  FieldParseError key valTxt ->
    [qq|Could not parse '{key}' field value: {valTxt}|]
  BadByr val lb ub ->
    [qq|Birth year (byr) is {val}, but must be at least {lb} and at most {ub}|]
  BadIyr val lb ub ->
    [qq|Issue year (iyr) is {val}, but must be at least {lb} and at most {ub}|]
  BadEyr val lb ub ->
    [qq|Expiration year (eyr) is {val}, but must be at least {lb} and at most {ub}|]
  MissingHgtSuffix txt ->
    [qq|Height (hgt) value is missing a 'cm' or 'in' suffix|]
  BadHgt val suffix lb ub ->
    [qq|Height (hgt) is {val}{suffix}, but must be at least {lb}{suffix} and at most {ub}{suffix}|]
  BadHcl txt ->
    [qq|Hair color (hcl) is {txt}, but must be a # followed by exactly six characters 0-9 or a-f|]
  BadEcl txt ->
    [qq|Eye color (ecl) is {txt}, but must be exactly one of: amb blu brn gry grn hzl oth|]
  BadPid txt ->
    [qq|Passport ID (pid) is {txt}, but must be a nine-digit number, including leading zeroes|]

-- | A passport is a collection of passport data fields that has
-- passed validation.
newtype Passport = Passport PassportData
  deriving Show

readField ::
  forall a f. ( Val.Validate f, Read a ) =>
  Text -> Text -> f (Seq PassportError) a
readField key valTxt =
  (maybe err (_Success #) . readMaybe . Text.unpack) valTxt
  where
    err = _Failure # Seq.singleton (FieldParseError key valTxt)

validatePassport1 ::
  PassportData ->
  Validation (PassportData, Seq PassportError) Passport
validatePassport1 pd = first (pd,) checks
  where
    checks =
      checkRequiredFields *> _Success # Passport pd

    checkRequiredFields =
      traverse_ checkField requiredFields

    checkField key =
      unless (Map.member key pd) (_Failure # [MissingRequiredField key])

    requiredFields :: Set Text
    requiredFields =
      Set.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

validatePassport2 :: PassportData -> Validation (PassportData, Seq PassportError) Passport
validatePassport2 pd = first (pd,) checks
  where
    checks =
      checkByr *>
      checkIyr *>
      checkEyr *>
      checkHgt *>
      checkHcl *>
      checkEcl *>
      checkPid *>
      _Success # Passport pd

    --
    -- Helpers
    --

    -- Helper which looks up the passport field (key), failing
    -- validation if it does not exist.
    look key =
      maybe
      (_Failure # Seq.singleton (MissingRequiredField key))
      (\val -> _Success # val)
      (Map.lookup key pd)

    check key f =
      look key `Val.bindValidation` f key

    checkBetween ::
      forall a. (Ord a, Read a) =>
      Text -> a -> a -> (a -> a -> a -> PassportError) ->
      Validation (Seq PassportError) ()
    checkBetween key lb ub errCons =
      look key `Val.bindValidation` \txt ->
      readField key txt `Val.bindValidation` \val ->
      unless (lb <= val && val <= ub) (_Failure # [errCons val lb ub])

    --
    -- Field checks
    --

    checkByr = checkBetween @Int "byr" 1920 2002 BadByr
    checkIyr = checkBetween @Int "iyr" 2010 2020 BadIyr
    checkEyr = checkBetween @Int "eyr" 2020 2030 BadEyr

    checkHgt = check "hgt" $ \key txt ->
      let
        -- try to validate height, provided the given suffix is
        -- actually a suffix of txt
        try suffix lowerBound upperBound =
          (Text.stripSuffix suffix txt <&>) $ \valTxt ->
          readField key valTxt `Val.bindValidation` \val ->
          unless (lowerBound <= val && val <= upperBound)
          (_Failure # [BadHgt val suffix lowerBound upperBound])

        errNoSuffix = _Failure # [MissingHgtSuffix txt]
      in
        fromMaybe errNoSuffix $ msum @[] [try "cm" 150 193, try "in" 59 76]

    checkHcl = check "hcl" $ \_key txt ->
      let validChars = ['a'..'f'] ++ ['0'..'9']
          checkCol col =
            Text.length col == 6 && Text.all (`elem` validChars) col
      in case Text.uncons txt of
        Just ((== '#') -> _, checkCol -> val) | val -> pure ()
        _ -> _Failure # [BadHcl txt]

    checkEcl = check "ecl" $ \_key txt ->
      unless
      (elem @[] txt ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
      (_Failure # [BadEcl txt])

    checkPid = check "pid" $ \_key txt ->
      unless
      (Text.length txt == 9 && Text.all (`elem` (['0'..'9'] :: [Char])) txt)
      (_Failure # [BadPid txt])

main :: IO ()
main = do
  pds <- parse

  putStrLn "# Invalid passports report (strict requirements)"
  let invalids =
        pds ^.. traverse . to validatePassport2 . to Val.toEither . _Left
  for (zip [1..] invalids) $ \(ipd, (pd, errs)) -> do
    putStrLn [qq|{ipd}: {pd} has {length errs} errors:|]
    for errs $ \err -> do
      putStrLn [qq|    {prettyPassportError err}|]

  putStrLn ""
  putStrLn "# Valid passports report"
  let countValidPassports val =
        lengthOf (traverse . to val . to Val.toEither . _Right) pds
      n1 = countValidPassports validatePassport1
      n2 = countValidPassports validatePassport2
  putStrLn [qq|Found {n1} valid passports using initial requirements.|]
  putStrLn [qq|Found {n2} valid passports using stricter requirements.|]
