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
  Policy { atLeast :: !Int
         , atMost :: !Int
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
      atLeast <- PCL.decimal <* P.single '-'
      atMost <- PCL.decimal <* P.single ' '
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

-- Policy says that ch must appear between atLeast and atMost
-- (inclusive) number of times in pass.
checkPolicy :: Policy -> Maybe Policy
checkPolicy pol =
  if valid then Just pol else Nothing
  where
    valid = pol ^. #atLeast <= numChars && numChars <= pol ^. #atMost
    numChars = Text.foldl step 0 (pol ^. #pass)
    step n c = if c == pol ^. #ch then n + 1 else n

main :: IO ()
main = do
  pols <- parse
  let
    checked = pols ^.. traverse . to checkPolicy
    n = sumOf (folded . traverse . to (const 1)) checked :: Int
  putStrLn [qq|Found {n} valid policies.|]
