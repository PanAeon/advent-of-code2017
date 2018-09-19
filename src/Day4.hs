module Day4
    (
    ) where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           System.IO
import           System.IO.Unsafe

task1Input = unsafePerformIO $ do
                handle <- openFile "/Users/edevi86/Downloads/passphrases.txt" ReadMode
                contents <- hGetContents handle
                pure $ lines contents

-- \x -> length x == length nub x
-- uncurry (==)  . (join (***) length) . (id &&& nub)

numCorrect =  length $ filter (uncurry (==)  . (join (***) length) . (id &&& nub)) $ words <$> task1Input


{-
no two words that are anagrams of each other - that is, a passphrase is invalid if
any word's letters can be rearranged to form any other word in the passphrase.

For example:

    abcde fghij is a valid passphrase.
    abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
    a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
    iiii oiii ooii oooi oooo is valid.
    oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

--}

simpleAnnagram :: String -> String -> Bool
simpleAnnagram = (. sort) . (==) . sort

numCorrect' =  length $ filter (uncurry (==)  . (join (***) length) . (id &&& nub)) $ (sort <$>) . words <$> task1Input
