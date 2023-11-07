-----------------------------------------------------------------------------------------------------------------------
-- Lambad
-- https://github.com/jotadiego/Lambad
-----------------------------------------------------------------------------------------------------------------------

-- Representation Encoded using Morse code -- an impractical way of handling text for an impractical setting

module Remorse (isMorse, toMorse, internationalMorseCode, morseToText, toMorseText) where

import Lambda
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ( fromJust )

-- Texts will be represented as a lambda expression with four variables - a terminal, a dit, a dah and a space.
-- A text will be formed by repeatedly the variable for the next character.
-- A single space is used to separate letters, two spaces to separate words.

-- Check whether a Lambda expression has the right format
isMorse :: Lambda -> Bool
isMorse = checkLambdaFormat 4 0

-- Convert Lambda expression to a string with the corresponding Morse code
toMorse :: Lambda -> Maybe String
toMorse l = if isMorse l
            then Just (reverse $ revMorseSeq l)
            else Nothing

-- Aux: Constructs the Morse code equivalent for a Lambda expression, but returns the result in reverse
revMorseSeq :: Lambda -> String
revMorseSeq (LAbs _ l)  = revMorseSeq l
revMorseSeq (LApp l1 l2) = case l1 of (LVar 1) -> '.' : revMorseSeq l2
                                      (LVar 2) -> '-' : revMorseSeq l2
                                      (LVar 3) -> ' ' : revMorseSeq l2
                                      _ -> ""
revMorseSeq _ = ""

-- Morse to Latin alphabet and numeral equivalences from the International Morse Code
internationalMorseCode :: Map String Char
internationalMorseCode = Map.fromList [
                             (".-", 'A'),
                             ("-...", 'B'),
                             ("-.-.", 'C'),
                             ("-..", 'D'),
                             (".", 'E'),
                             ("..-.", 'F'),
                             ("--.", 'G'),
                             ("....", 'H'),
                             ("..", 'I'),
                             (".---", 'J'),
                             ("-.-", 'K'),
                             (".-..", 'L'),
                             ("--", 'M'),
                             ("-.", 'N'),
                             ("---", 'O'),
                             (".--.", 'P'),
                             ("--.-", 'Q'),
                             (".-.", 'R'),
                             ("...", 'S'),
                             ("-", 'T'),
                             ("..-", 'U'),
                             ("...-", 'V'),
                             (".--", 'W'),
                             ("-..-", 'X'),
                             ("-.--", 'Y'),
                             ("--..", 'Z'),
                             ("/", ' '),
                             ("-----", '0'),
                             (".----", '1'),
                             ("..---", '2'),
                             ("...--", '3'),
                             ("....-", '4'),
                             (".....", '5'),
                             ("-....", '6'),
                             ("--...", '7'),
                             ("---..", '8'),
                             ("----.", '9'),
                             ("...---...", '!') -- The 'SOS' code is not properly part of the International Morse Code
                                                -- alphabet, but it felt wrong not to encode it somehow
                         ]

-- Aux: Extract a character from a string if not a space
auxSplitMorse :: String -> (String, String)
auxSplitMorse (' ' : s) = ("", s)
auxSplitMorse (x : s) = ([x], s)
auxSplitMorse "" = ("", "")

-- Aux: List of substrings split by spaces
auxSplitMorse2 :: String -> String -> [String]
auxSplitMorse2 "" prev = [prev]
auxSplitMorse2 s prev = if sub == ""
                        then prev : auxSplitMorse2 next ""
                        else auxSplitMorse2 next (prev ++ sub)
                        where (sub, next) = auxSplitMorse s

-- Replace empty strings in a sequence with the string "/"
auxSplitMorse3 :: [String] -> [String]
auxSplitMorse3 = map (\s -> if s == "" then "/" else s)

morseToText :: String -> String
morseToText m = map (\x -> fromJust $ Map.lookup x internationalMorseCode) (auxSplitMorse3 $ auxSplitMorse2 m "")

toMorseText :: Lambda -> Maybe String
toMorseText l = case toMorse l of Just m -> Just (morseToText m)
                                  Nothing -> Nothing