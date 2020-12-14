#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [containers interpolatedstring-perl6 generic-lens lens megaparsec tasty tasty-hunit text])"

{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language DuplicateRecordFields #-}
{-# language RecordWildCards #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}

import Control.Lens

import Control.Applicative ((<|>))
import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Control.Monad.State (State, execState)
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
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
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitSuccess))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.InterpolatedString.Perl6 (qq)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PCL

data Bag =
  Bag { adjective :: !Text
      , color     :: !Text
      }
  deriving (Eq, Generic, Ord, Show)

data Rule =
  Rule { bag      :: !Bag
       , contents :: !(Seq Contents)
       }
  deriving (Eq, Generic, Show)

data Contents =
  Contents { num :: !Int
           , bag :: !Bag
           }
  deriving (Eq, Generic, Show)

rule :: Iso' (Bag, Seq Contents) Rule
rule = iso toRule toAssoc
  where
    toRule (bag', contents') = Rule { bag = bag', contents = contents' }
    toAssoc r = (r ^. #bag, r ^. #contents)

-- | Maps bag to its direct contents.
newtype RuleMap = RuleMap { unRuleMap :: Map Bag (Seq Contents) }
  deriving (Eq, Generic, Show)

-- | Maps bag to all bags in which it can be directly contained.
newtype ParentMap = ParentMap { unParentMap :: Map Bag (Set Bag) }
  deriving (Eq, Generic, Show)

-- | Create ParentMap from a RuleMap by inverting it and disregarding
-- the the exact number of bags in each 'Contents'; just knowing there
-- is a bag of that type is enough.
fromRuleMap :: RuleMap -> ParentMap
fromRuleMap =
  ParentMap . invertMap (<>) Set.singleton . bagMap . unRuleMap
  where
    bagMap =
      over (traverse . traverse) (view #bag)

type Parser = P.Parsec Void Text

-- Parser for whole input file contents
parseInput :: Parser [Rule]
parseInput =
  parseRule `P.sepEndBy` PC.newline

-- | Unit tests for `parseInput`.
tests_parseInput :: TestTree
tests_parseInput =
  testGroup "parse input"
  [ let txt = "plaid tan bags contain 5 faded brown bags, 3 bright bronze bags.\n"
        res = [ Rule
                (Bag "plaid" "tan")
                [ Contents 5 (Bag "faded" "brown")
                , Contents 3 (Bag "bright" "bronze")
                ]
              ]
    in namedParseTest "with newline" txt parseInput res

  , let txt = "plaid tan bags contain 5 faded brown bags, 3 tan yellow bags.\nlumpy green bags contain 2 fat blue bags, 1 bright bronze bag."
        res = [ Rule
                (Bag "plaid" "tan")
                [ Contents 5 (Bag "faded" "brown")
                , Contents 3 (Bag "tan" "yellow")
                ]
              , Rule
                (Bag "lumpy" "green")
                [ Contents 2 (Bag "fat" "blue")
                , Contents 1 (Bag "bright" "bronze")
                ]
              ]
    in namedParseTest "double" txt parseInput res
  ]

parseRule :: Parser Rule
parseRule = do
  bag <- parseBag <* PC.string "contain" <* PC.hspace

  let noOtherBags = [] <$ PC.string "no other bags"
      someBags = parseContents `P.sepBy` (PC.char ',' <* PC.hspace)
  contents <- Seq.fromList <$> (noOtherBags <|> someBags) <* PC.char '.'

  pure (Rule {..})

-- | Unit tests for `parseRule`.
tests_parseRule :: TestTree
tests_parseRule =
  testGroup "parse rule"
  [ let txt = "bright bronze bags contain no other bags."
        res = Rule (Bag "bright" "bronze") []
    in parseTest txt parseRule res

  , let txt = "plaid tan bags contain 5 faded brown bags, 3 bright bronze bags."
        res = Rule
              (Bag "plaid" "tan")
              [ Contents 5 (Bag "faded" "brown")
              , Contents 3 (Bag "bright" "bronze")
              ]
    in parseTest txt parseRule res

  , let txt = "fat brown bags contain 1 dim yellow bag, 300 faded green bags."
        res = Rule
              (Bag "fat" "brown")
              [ Contents 1 (Bag "dim" "yellow")
              , Contents 300 (Bag "faded" "green")
              ]
    in parseTest txt parseRule res

  ]

parseBag :: Parser Bag
parseBag = do
  adjective <- Text.pack <$> P.some PC.letterChar <* PC.hspace
  color     <- Text.pack <$> P.some PC.letterChar <* PC.hspace
  _         <- PC.string "bag" <* P.optional (PC.char 's') <* PC.hspace
  pure (Bag adjective color)

-- | Unit tests for `parseBag`.
tests_parseBag :: TestTree
tests_parseBag =
  testGroup "parse bag"
  [ parseTest "faded tan bags"    parseBag (Bag "faded" "tan")
  , parseTest "garish yellow bag" parseBag (Bag "garish" "yellow")
  ]

parseContents :: Parser Contents
parseContents = do
  num <- PCL.decimal <* PC.hspace
  bag <- parseBag
  pure (Contents num bag)

-- Read and parse standard input
parseStdin :: IO RuleMap
parseStdin =
  parse . decodeUtf8 <$> B.getContents
  where
    mkMap :: [Rule] -> RuleMap
    mkMap =
      RuleMap . Map.fromList . toListOf (traverse . from rule)
    err =
      error . show
    parse =
      either err mkMap . P.runParser parseInput "<stdin>"

namedParseTest ::
  (Eq a, Show a) => String -> Text -> Parser a -> a -> TestTree
namedParseTest name txt p res =
  testCase name $ P.runParser p "" txt @?= Right res

parseTest :: (Eq a, Show a) => Text -> Parser a -> a -> TestTree
parseTest txt p res =
  namedParseTest (Text.unpack txt) txt p res

-- | Invert a map, so that each value in the map becomes a key in the
-- new map pointing to all its original keys.
invertMap ::
  ( Ord b, Foldable f )
  => (g a -> g a -> g a) -> (a -> g a) -> Map a (f b) -> Map b (g a)
invertMap comb one =
  flip execState mempty . traverse_ addVal . Map.assocs
  where
    addVal (v, ks) =
      traverse_ (addKey v) ks
    addKey v k =
      id %= Map.insertWith comb k (one v)

transitiveParents :: ParentMap -> Bag -> Set Bag
transitiveParents (unParentMap -> pm) =
  flip execState mempty . addBag
  where
    look :: Bag -> Set Bag
    look = fromMaybe mempty . flip Map.lookup pm

    addBag :: Bag -> State (Set Bag) ()
    addBag bag = case look bag of
      [] -> pure ()
      parents -> do
        unCheckedBags <- (parents `Set.difference`) <$> use id
        id <>= parents
        traverse_ addBag unCheckedBags

transitiveCount :: RuleMap -> Bag -> Int
transitiveCount (unRuleMap -> rm) =
  addBag
  where
    look :: Bag -> Seq Contents
    look = fromMaybe mempty . flip Map.lookup rm

    addBag :: Bag -> Int
    addBag =
      (1 +) . sumOf (traverse . to addContents) . look

    addContents :: Contents -> Int
    addContents c =
      (c ^. #num) * addBag (c ^. #bag)

-- | The tree of all tests in this file.
tests :: TestTree
tests = testGroup "main" [ tests_parseBag, tests_parseRule, tests_parseInput ]

-- | Run all tests.
testProg :: IO ()
testProg =
  defaultMain tests

runProg :: IO ()
runProg = do
  ruleMap <- parseStdin
  let
    parentMap = fromRuleMap ruleMap
    bag = Bag "shiny" "gold"
    numContainers = Set.size (transitiveParents parentMap bag)
    numBagsInside = transitiveCount ruleMap bag - 1

  putStrLn [qq|Found {numContainers} containers for shiny gold bag.|]
  putStrLn [qq|Need {numBagsInside} bags inside shiny gold bag.|]

main :: IO ()
main = do
  testProg `catch` \e -> do
    when (e == ExitSuccess) runProg
    throwIO e
