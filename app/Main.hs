module Main where
import  Exercise3
import  Exercise4
import Test.QuickCheck
import Data.List
main :: IO ()
main = do
        print $ "Exercise3"
        -- print $ quickSort props
        print $  getNames $ quicksortProps funsToSort