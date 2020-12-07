#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [interpolatedstring-perl6 generic-lens lens tasty tasty-hunit])"

{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language QuasiQuotes #-}

import Control.Lens

import Control.Exception (catch, throwIO)
import Control.Monad (guard, when)
import Data.Generics.Labels ()
import Data.Traversable (for)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitSuccess))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.InterpolatedString.Perl6 (qq)

data Size =
  Size { width :: !Int
       , height :: !Int
       }
  deriving (Eq, Generic, Show)

data Pos =
  Pos { col :: !Int
      , row :: !Int
      }
  deriving (Eq, Generic, Show)

data RelPos =
  RelPos { col :: !Int
         , row :: !Int
         }
  deriving (Eq, Generic, Show)

posAdd :: Pos -> RelPos -> Pos
posAdd pos relPos =
  pos
  & #col %~ (+ (relPos ^. #col))
  & #row %~ (+ (relPos ^. #row))

data Forest=
  Forest { grid :: Vector Char
         , size :: Size
         }
  deriving (Eq, Generic, Show)

-- | Lookup what's in the forest at the given position.
--
-- The forest keeps repeating forever in the x (width) dimension, but
-- has finite height.
forestLookup :: Forest -> Pos -> Maybe Char
forestLookup forest pos = do
  let w   = forest ^. #size . #width
      h   = forest ^. #size . #height
      y   = pos ^. #row
      x   = pos ^. #col
      loc = (x `mod` w) + (w * y)
  guard (0 <= y && y < h)
  pure $ forest ^. #grid . to (VG.! loc)

-- | Small forest for unit tests.
-- @
--   abc
--   def
-- @
tinyForest1 :: Forest
tinyForest1 =
  Forest { grid = VG.fromList ['a'..'f']
         , size = Size { width = 3, height = 2 }
         }

-- | Encoded version of `tinyForest1`, for parser unit tests.
tinyForest1Str :: String
tinyForest1Str =
  "abc\ndef\n"

-- | Unit tests for `forestLookup`.
tests_forestLookup :: TestTree
tests_forestLookup =
  testGroup "Forest lookup"
  [ testCase "Just 'a' @ Pos 0 0" $ fl (Pos 0 0) @?= Just 'a'
  , testCase "Just 'a' @ Pos 1 0" $ fl (Pos 1 0) @?= Just 'b'
  , testCase "Just 'f' @ Pos 2 1" $ fl (Pos 2 1) @?= Just 'f'
  , testCase "Nothing @ Pos 2 2" $ fl (Pos 2 2) @?= Nothing
  , testCase "Just 'c' @ Pos (-1) 0" $ fl (Pos (-1) 0) @?= Just 'c'
  , testCase "Just 'e' @ Pos (-2) 1" $ fl (Pos (-2) 1) @?= Just 'e'
  , testCase "Nothing @ Pos (-2) (-1)" $ fl (Pos (-2) (-1)) @?= Nothing
  ]
  where
    fl = forestLookup tinyForest1

-- | Errors we might get while parsing the string representation of a
-- forest.
data ParseForestError
  = NoForestRowsFound
  | DifferentRowSizesFound (Vector Int)
  deriving (Eq, Show)

-- | Text versions of `ParseForestError`, for humans.
prettyParseForestErr :: ParseForestError -> String
prettyParseForestErr = \case
  NoForestRowsFound ->
    "No forest rows found"
  DifferentRowSizesFound ws ->
    "Rows of different size detected. Here are all row sizes: " <> show ws

-- | Parse the contents of a forest file.
parseForest :: String -> Either ParseForestError Forest
parseForest str = do
  -- A little extra bookkeeping here with type signatures and
  -- conversion, because the inner vectors (rows) are unboxed and the
  -- outer are boxed.
  let arr = VG.fromList (VG.fromList <$> lines str) :: V.Vector (Vector Char)
      h = VG.length arr
  when (h == 0) $ Left NoForestRowsFound
  let w  = VG.length (arr VG.! 0)
      ws = VG.convert (VG.length <$> arr)
  when (not (VG.all (w ==) ws)) $ Left (DifferentRowSizesFound ws)
  when (w == 0) $ Left NoForestRowsFound
  pure $ Forest { grid = VU.concat (VG.toList arr)
                , size = Size { width = w, height = h } }

-- | Unit tests for `parseForest`.
tests_parseForest :: TestTree
tests_parseForest =
  testGroup "Forest parsing"

  [ testCase "parse tiny forest" $
    parseForest tinyForest1Str @?= Right tinyForest1

  , testCase "parse tiny forest" $
    parseForest "a\ncd\nefg" @?= Left (DifferentRowSizesFound [1,2,3])

  , testCase "parse tiny forest" $
    parseForest "" @?= Left NoForestRowsFound
  ]

-- | Read standard input and parse as `Forest`.
parse :: IO Forest
parse =
  either err id . parseForest <$> getContents
  where
    err =
      error . prettyParseForestErr

-- | Look up all the forest locations along the path given by starting
-- position and change in positions for each step.
lookupPath :: Pos -> RelPos -> Forest -> [Char]
lookupPath startPos diffPos forest =
  go startPos mempty
  where
    go pos acc =
      maybe
      acc
      (\ch -> go (posAdd pos diffPos) (acc |> ch))
      (forestLookup forest pos)

-- | Count trees along the path given by starting position and change
-- in position for each step.
countTrees :: Pos -> Forest -> RelPos -> Int
countTrees startPos forest diffPos =
  length $ filter (== '#') $ lookupPath startPos diffPos forest

-- | The tree of all tests in this file.
tests :: TestTree
tests =
  testGroup "main"
  [ tests_forestLookup
  , tests_parseForest ]

-- | Run all tests.
testProg :: IO ()
testProg =
  defaultMain tests

-- | Run program normally. Expects the contents of a forest file on
-- standard input and produces a human-readable report on stdout.
runProg :: IO ()
runProg = do
  let cfgs = [ RelPos 1 1
             , RelPos 3 1
             , RelPos 5 1
             , RelPos 7 1
             , RelPos 1 2
             ] :: [RelPos]

  forest <- parse
  let count = countTrees (Pos 0 0) forest
  treeProd <- fmap product . for cfgs $ \diffPos -> do
    let n = count diffPos
        r = diffPos ^. #row
        d = diffPos ^. #col
    putStrLn [qq|Found {n} trees in path using Right {r}, Down {d}.|]
    pure n
  putStrLn [qq|The final product of trees encountered is {treeProd}.|]

-- | Run all the tests, then the normal program, provided all tests
-- passed. It's a bit weird, but useful during development.
main :: IO ()
main =
  testProg `catch` (\e -> if e == ExitSuccess then runProg else pure () >> throwIO e)
