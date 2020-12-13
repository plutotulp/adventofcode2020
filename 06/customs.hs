#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [containers interpolatedstring-perl6 lens text])"

{-# language FlexibleContexts #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}

import Control.Lens

import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq)
import Data.Text (Text, pack, unpack)
import Text.InterpolatedString.Perl6 (qq)

newtype Group = Grp { unGroup :: Seq Person }
  deriving (Eq, Show)

newtype Person = Psn { unPerson :: Text }
  deriving (Eq, Show)

parse :: String -> Seq Group
parse =
  cleanup . foldlOf folded go (Grp [], []) . lines
  where
    go (g@(Grp []), gs) ""            = (g                  , gs     )
    go (g         , gs) ""            = (Grp []             , gs |> g)
    go (Grp ps    , gs) (pack -> txt) = (Grp (ps |> Psn txt), gs     )

    cleanup (Grp [], gs) = gs
    cleanup (g     , gs) = gs |> g

countUnique :: (Foldable f, Enum a) => f a -> Int
countUnique =
  IntSet.size . IntSet.fromList . fmap fromEnum . toList

uniqueAnswers :: Group -> Int
uniqueAnswers =
  countUnique . concatMap (unpack . unPerson) . unGroup

countIntersected ::
  (Foldable f, Foldable g, Enum a) => f (g a) -> Int
countIntersected g =
  let ps =
        (fmap (IntSet.fromList . fmap fromEnum . toList) . toList) g
  in case ps of
    [] -> 0
    (a:as) -> IntSet.size (foldlOf folded IntSet.intersection a as)

agreedAnswers :: Group -> Int
agreedAnswers =
  countIntersected . fmap (unpack . unPerson) . unGroup

main :: IO ()
main = do
  groups <- parse <$> getContents
  let n1 = sum (uniqueAnswers <$> groups)
  putStrLn [qq|The sum of counts for 'yes' is {n1}.|]

  let n2 = sum (agreedAnswers <$> groups)
  putStrLn [qq|The sum of 'yes' answers agreed upon by everyone in the group is {n2}.|]
