#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: with pkgs; [interpolatedstring-perl6])"

{-# language QuasiQuotes #-}

import Data.Maybe (mapMaybe)
import Text.InterpolatedString.Perl6 (qq)

desiredSum :: Int
desiredSum = 2020

sumIs :: Int -> (Int, Int, Int) -> Maybe (Int, Int, Int)
sumIs target tup@(x, y, z) =
  if x + y + z == target then Just tup else Nothing

parse :: FilePath -> IO [Int]
parse =
  fmap (fmap read . lines) . readFile

search :: [Int] -> [(Int, Int, Int)]
search nums =
  mconcat [check k j | k <- [1..(n-1)], j <- [k+1..(n-1)]]
  where
    n = length nums
    check k j = mapMaybe (sumIs desiredSum) $ zip3 nums (drop k nums) (drop j nums)

handle :: [(Int, Int, Int)] -> IO ()
handle res= putStrLn $ case res of
  [(x, y, z)] ->
    [qq|$x + $y + $z = $desiredSum, and $x * $y * $z = {x*y*z}.|]
  [] ->
    "Could not find a solution."
  ms ->
    [qq|"Multiple solutions found, which probably means there's an error in the program. Solutions: $ms."|]

main :: IO ()
main =
  handle =<< search <$> parse "input"
