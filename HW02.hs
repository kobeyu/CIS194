{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List
import Control.Arrow((&&&))
import Data.Function(on)
import Safe

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (w:ws) hand
	| w `elem` hand = formableBy ws (delete w hand)
	| otherwise = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _ = False
wordFitsTemplate _ _ [] = False
wordFitsTemplate ('?':tmp) h (w:ws) = w `elem` h && wordFitsTemplate tmp (delete w h) ws
wordFitsTemplate (t:ts) h (w:ws) = t == w && wordFitsTemplate ts h ws

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tmp hand = filter (wordFitsTemplate tmp hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

bestWords :: [String] -> [String]
bestWords = map snd . headDef [] . groupBy ((==) `on` fst) . sortBy (flip (compare `on` fst) ) . map (scrabbleValueWord&&&id)

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate = (reduce .) . zipWith f
	where 
		f :: Char -> Char -> (Int,Int)
		f 'D' c = (2*scrabbleValue c,1)
		f 'T' c = (3*scrabbleValue c,1)
		f '2' c = (scrabbleValue c, 2)
		f '3' c = (scrabbleValue c, 3)
		f  _  c = (scrabbleValue c,1)
		reduce :: [(Int,Int)] -> Int
		reduce fs = (sum . map fst $ fs) * (product . map snd $ fs)

