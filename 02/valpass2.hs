#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [interpolatedstring-perl6 megaparsec text generic-lens lens])"

{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}

import Control.Lens

import qualified Control.Applicative.Combinators as Comb
import qualified Data.ByteString as B
import Data.Generics.Labels ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.InterpolatedString.Perl6 (qq)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PCL

data Policy =
  Policy { pos1 :: !Int
         , pos2 :: !Int
         , ch :: !Char
         , pass :: !Text
         }
  deriving (Generic, Show)

-- Parser for whole input file contents
parseInput :: P.Parsec Void Text [Policy]
parseInput =
  parsePolicy `Comb.sepEndBy` PC.newline
  where
    parsePolicy = do
      pos1 <- PCL.decimal <* P.single '-'
      pos2 <- PCL.decimal <* P.single ' '
      ch <- PC.letterChar <* PC.string ": "
      pass <- P.takeWhile1P Nothing (/= '\n')
      pure Policy{..}

-- Read and parse standard input
parse :: IO (Seq Policy)
parse =
  runParser . decodeUtf8 <$> B.getContents
  where
    runParser txt =
      either (error.show) Seq.fromList
      $ P.runParser parseInput "<stdin>" txt

-- Policy says that ch must appear exactly once in either pos1 or
-- pos2. If it appears in neither, the password is invalid. If it
-- appears in both, the password is also invalid.
checkPolicy :: Policy -> Maybe Policy
checkPolicy pol = do
  let look = lookupChar (pol ^. #pass)
      enumCh c = if c == pol ^. #ch then 1 else 0

      -- defining out-of-bounds lookups as not matching the policy
      -- character, so looking for character 'c' at position 999 in
      -- the password "secret" yields 0, but looking at position 3
      -- yields 1.
      enumAt = maybe (0::Int) enumCh . look

      x1 = enumAt (pol ^. #pos1 . to pred)
      x2 = enumAt (pol ^. #pos2 . to pred)

      -- account for edge case where the two positions are the same,
      -- meaning we actually want to see the character exactly twice.
      samePos = pol ^. #pos1 == pol ^. #pos2

  case (samePos, x1 + x2) of
    (True,  2) -> Just pol
    (False, 1) -> Just pol
    _ -> Nothing

lookupChar :: Text -> Int -> Maybe Char
lookupChar txt i =
  if Text.length txt - 1 < i then Nothing else Just (Text.index txt i)

main :: IO ()
main = do
  pols <- parse
  let
    checked = pols ^.. traverse . to checkPolicy
    n = sumOf (folded . traverse . to (const 1)) checked :: Int
  putStrLn [qq|Found {n} valid policies.|]
