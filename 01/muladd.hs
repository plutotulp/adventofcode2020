#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [interpolatedstring-perl6])"

{-# language QuasiQuotes #-}

import Data.Maybe (mapMaybe)
import Text.InterpolatedString.Perl6 (qq)

desiredSum :: Int
desiredSum = 2020

sumIs :: Int -> (Int, Int) -> Maybe (Int, Int)
sumIs target pair@(x, y) =
  if x + y == target then Just pair else Nothing

parse :: FilePath -> IO [Int]
parse =
  fmap (fmap read . lines) . readFile

search :: [Int] -> [(Int, Int)]
search nums =
  mconcat (check <$> [1..(length nums - 1)])
  where
    check = mapMaybe (sumIs desiredSum) . zip nums . flip drop nums

handle :: [(Int, Int)] -> IO ()
handle res = putStrLn $ case res of
  [(x, y)] ->
    [qq|$x + $y = $desiredSum, and $x * $y = {x*y}.|]
  [] ->
    "Could not find a solution."
  ms ->
    [qq|"Multiple solutions found, which probably means there's an error in the program. Solutions: $ms."|]

main :: IO ()
main =
  handle =<< search <$> parse "input"
