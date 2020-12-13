#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [containers interpolatedstring-perl6 generic-lens lens mtl tasty tasty-hunit])"

{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}

import Control.Lens

import Control.Exception (catch, throwIO)
import Control.Monad (ap, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (evalState)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_, for_, toList)
import Data.Generics.Labels ()
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitSuccess))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.InterpolatedString.Perl6 (qq)

newtype Row = Row Int
  deriving (Enum, Eq, Integral, Num, Ord, Show, Real)

newtype Col = Col Int
  deriving (Enum, Eq, Integral, Num, Ord, Show, Real)

newtype SeatId = SeatId { unSeatId :: Int }
  deriving (Enum, Eq, Integral, Num, Ord, Show, Real)

data Loc = Loc
  { row :: !Row
  , col :: !Col
  , seatId :: !SeatId
  }
  deriving (Eq, Generic, Show)

data DecodeState = DecodeState
  { rlb :: !Row -- ^ Row lower bound
  , rub :: !Row -- ^ Row upper bound
  , clb :: !Col -- ^ Column lower bound
  , cub :: !Col -- ^ Column upper bound
  }
  deriving (Eq, Generic, Show)

data DecodeError
  = IncompletePartitioning String DecodeState
  | InvalidInstruction Char String DecodeState
  deriving (Eq, Show)

decodeLoc :: String -> Either DecodeError Loc
decodeLoc str =
  evalState (runExceptT (traverse_ step str *> mkLoc)) initState
  where
    initState =
      DecodeState { rlb = Row 0, rub = Row 128, clb = Col 0, cub = Col 8 }

    mkLoc = do
      st@(DecodeState rlb'@(Row r) rub' clb'@(Col c) cub') <- use id
      if rub' - rlb' == 1 && cub' - clb' == 1
      then pure (Loc { row = rlb', col = clb', seatId = SeatId (r * 8 + c) })
      else throwError (IncompletePartitioning str st)

    getRs = do
      rub' <- use #rub
      rlb' <- use #rlb
      pure $ (rub' - rlb') `div` 2

    getCs = do
      cub' <- use #cub
      clb' <- use #clb
      pure $ (cub' - clb') `div` 2

    step c =
      case c of
        'F' -> getRs >>= (#rub -=)
        'B' -> getRs >>= (#rlb +=)
        'L' -> getCs >>= (#cub -=)
        'R' -> getCs >>= (#clb +=)
        _ -> use id >>= \st -> throwError (InvalidInstruction c str st)

tests_locs :: TestTree
tests_locs =
  testGroup "decode seating"

  [ let str = "FBFBBFFRLR" in testCase str $
    decodeLoc str @?= Right (Loc (Row 44) (Col 5) (SeatId 357))

  , let str = "BFFFBBFRRR" in testCase str $
    decodeLoc str @?= Right (Loc (Row 70) (Col 7) (SeatId 567))

  , let str = "FFFBBBFRRR" in testCase str $
    decodeLoc str @?= Right (Loc (Row 14) (Col 7) (SeatId 119))

  , let str = "BBFFBBFRLL" in testCase str $
    decodeLoc str @?= Right (Loc (Row 102) (Col 4) (SeatId 820))

  , let str = "BBFFBBFRLQ" in testCase (str ++ " invalid instr.") $
    decodeLoc str @?= Left (InvalidInstruction 'Q' str (DecodeState (Row 102) (Row 103) (Col 4) (Col 6)))

  , let str = "BBFFBBFRL" in testCase (str ++ " incomplete") $
    decodeLoc str @?= Left (IncompletePartitioning str (DecodeState (Row 102) (Row 103) (Col 4) (Col 6)))

  ]

-- | The tree of all tests in this file.
tests :: TestTree
tests = testGroup "main" [ tests_locs ]

-- | Run all tests.
testProg :: IO ()
testProg = defaultMain tests

sortInts :: [Int] -> [Int]
sortInts = IntSet.toAscList . IntSet.fromList

pairs :: [a] -> [(a, a)]
pairs = ap zip tail

findHoles :: (Eq a, Num a) => [(a, a)] -> Seq a
findHoles =
  let step (x, y) acc
        | x + 2 == y = acc |> x + 1
        | otherwise  = acc
  in foldr step []

findSeatHoles :: forall f. Foldable f => f Loc -> Seq SeatId
findSeatHoles = fmap SeatId . findHoles . pairs . sortInts . toInts
  where
    toInts :: f Loc -> [Int]
    toInts =
      toListOf (folded . #seatId . to unSeatId)

data LocateSeatError
  = NotFound
  | TooManyFound (Seq SeatId)

findMySeat :: Foldable f => f Loc -> Either LocateSeatError SeatId
findMySeat locs = case findSeatHoles locs of
  []    -> Left NotFound
  [sid] -> Right sid
  sids  -> Left (TooManyFound sids)

runProg :: IO ()
runProg = do
  -- Get input from stdin and decode into locations.
  (Seq.fromList -> badScans, Seq.fromList -> locs) <-
    partitionEithers . fmap decodeLoc . lines <$> getContents

  let nb = Seq.length badScans
  when (0 < nb) $ do
    putStrLn [qq|Failed to decode {nb} boarding passes. |]
    for_ (zip [(1::Int)..] (toList badScans)) $ \(ierr, err) ->
      putStrLn [qq|{ierr}: {err}|]

  let n = Seq.length locs
  putStrLn [qq|Decoded {n} boarding passes.|]
  when (0 < n) $ do

    -- Safe Just because 0 < n
    let Just (SeatId maxSeatId) = maximumOf (traverse . #seatId) locs
    putStrLn [qq|Maximum seat ID: {maxSeatId}.|]

  case findMySeat locs of
    Left NotFound ->
      putStrLn [qq|Could not find my seat!|]
    Left (TooManyFound (fmap unSeatId . toList -> sids)) ->
      putStrLn [qq|Found more than one free seat IDs: {sids}|]
    Right (unSeatId -> sid) ->
      putStrLn [qq|Found my seat ID: {sid}.|]

main :: IO ()
main = do
  testProg `catch` \e -> do
    when (e == ExitSuccess) runProg
    throwIO e
