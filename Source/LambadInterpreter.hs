-----------------------------------------------------------------------------------------------------------------------
-- Lambad
-- https://github.com/jotadiego/Lambad
-----------------------------------------------------------------------------------------------------------------------

-- An interpreter for Lambad

{-# LANGUAGE OverloadedStrings #-}

import Lambad
import Lambda
import Remorse

import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import System.IO
import qualified Control.Monad

-- Modes, indicated by the first 
modeMessage :: String -> String
modeMessage "v" = "Representing Lambad program in the Verbose Lambad format..."
modeMessage "w" = "Representing Lambad program in a normalized Shortened Lambad format..."
modeMessage "l" = "Representing Lambad program as a lambda expression..."
modeMessage "x" = "Computing the reduced Lambda expression..."
modeMessage "xs" = "(SAFE) Computing the reduced Lambda expression..."
modeMessage "b" = "Computing the reduced Lambda expression as a boolean..."
modeMessage "bs" = "(SAFE) Computing the reduced Lambda expression as a boolean..."
modeMessage "n" = "Computing the reduced Lambda expression as a number..."
modeMessage "ns" = "(SAFE) Computing the reduced Lambda expression as a number..."
modeMessage "m" = "Computing the reduced Lambda expression as a morse-encoded string..."
modeMessage "ms" = "(SAFE) Computing the reduced Lambda expression as a morse-encoded..."
modeMessage "t" = "Computing the reduced Lambda expression as text..."
modeMessage "ts" = "(SAFE) Computing the reduced Lambda expression as text..."
modeMessage _ = error "Unknown mode"

-- Get the Lambad program
getProgram :: [String] -> IO Text
getProgram (fileName : args) = do program <- TIO.readFile fileName
                                  addArgs program 1 args



-- Add extra arguments specified in the command line
addArgs :: Text -> Int -> [String] -> IO Text
addArgs program index (fileName : args) = do arg <- TIO.readFile fileName
                                             addArgs (substituteArg program index arg) (index + 1) args
addArgs program index [] = return program

-- Substitute  `{n}` tags with the code for the n-th argument
substituteArg :: Text -> Int -> Text -> Text
substituteArg program index arg = replace (pack $ "{" ++ show index ++ "}") arg program

-- Fix text output to include the correct symbols for × (multiplication sign) and λ (lambda)
fixTextTimes :: String -> Text
fixTextTimes s = replace (pack "x") (pack "×") (pack s)

fixTextLambda :: String -> Text
fixTextLambda s = replace (pack "L") (pack "λ") (pack s)

safeReduceError :: Text
safeReduceError = pack "ERROR - lambda expression reduction stopped to prevent likely infinite loop"

typeFormatError :: Text
typeFormatError = pack "ERROR - the resulting expression does not have a valid format for the intended type"

-- Extract a safe Lambda as text
printSafeLambda :: Maybe Lambda -> Text
printSafeLambda (Just l) = fixTextLambda $ show l
printSafeLambda _ = safeReduceError

-- Show a result if its format was valid (used for lambda expressions interpreted as naturals, booleans or text)
showIfValid :: Show a => Maybe a -> Text
showIfValid (Just x) = pack $ show x
showIfValid _ = typeFormatError

-- Apply interpretation operations to safely-reduced strings
convertSafeLambda :: (Show a) => Maybe Lambda -> (Lambda -> Maybe a) -> Text
convertSafeLambda (Just l) f = showIfValid (f l)
convertSafeLambda _ _ = safeReduceError

-- Runs a program in a given mode
run :: LambadProgram -> String -> Text
    -- Get Verbose Lambad representation
run p "v" = fixTextTimes $ showVerbose p
    -- Get a Shortened Lambad representation
run p "w" = fixTextTimes $ show p
    -- Get unreduced lambda expression
run p "l" = fixTextLambda $ show $ toLambda p
    -- Get reduced lambda expression
run p "x" = fixTextLambda $ show $ maxReduce (toLambda p)
    -- Get safely reduced lambda expression
run p "xs" = printSafeLambda $ safeReduce (toLambda p)
    -- Evaluate reduced lambda expression as a Boolean
run p "b" = showIfValid $ toBoolean $ maxReduce (toLambda p)
    -- Evaluate safely reduced lambda expression as a Boolean
run p "bs" = convertSafeLambda (safeReduce (toLambda p)) toBoolean
    -- Evaluate reduced lambda expression as a natural number
run p "n" = showIfValid $ toNatural $ maxReduce (toLambda p)
    -- Evaluate safely reduced lambda expression as a natural number
run p "ns" = convertSafeLambda (safeReduce (toLambda p)) toNatural
    -- Evaluate reduced lambda expression as a Morse-encoded string
run p "m" = showIfValid $ toMorseText $ maxReduce (toLambda p)
    -- Evaluate safely reduced lambda expression as a Morse-encoded string
run p "ms" = convertSafeLambda (safeReduce (toLambda p)) toMorseText
    -- Evaluate reduced lambda expression as a byte-encoded string
run p "t" = case toText $ maxReduce (toLambda p) of
                Just t -> t
                _ -> typeFormatError
-- Evaluate safely reduced lambda expression as a byte-encoded string
-- I'd've used the scary `>>=`
run p "ts" = case safeReduce (toLambda p) of
                Nothing -> safeReduceError
                Just l -> case toText l of
                    Just t -> t
                    _ -> typeFormatError

---

-- If I didn't want to get different error messages for expressions that couldn't be safely reduced and expressions
-- that were safely reduced but failed a later format check, I could have written modes 'bs', 'ns', 'ms' and 'ts'
-- using the scary but actually quite practical `>>=` operator.
-- For instance, `safeReduce (toLambda p) >>= toBoolean` would return us the boolean (wrapped in a 'Just') if both
-- operations worked or 'Nothing' if either failed.

-- Using an 'error monad' would have allowed me to use that `>>=` notation to make those functions less cumbersome
-- although that would have force me to include those errors within the functions defined in the Lambda.hs module
-- (which wouldn't be too elegant) or define alternative functions here (which wouldn't be less work).

---

-- For some reason, the '×' character is sometimes misread as the sequence "├ù".
-- I wasted _hours_ trying to locate the source of the problem, to no avail.
-- Forcefully replacing the misread sequence for the right value is terribly inelegant but I'd rather not keep
-- struggling with Haskell's awful Unicode support.
fixWeirdEncodingError :: Text -> Text
fixWeirdEncodingError = replace (pack "\9500\249") (pack "\xd7")

main :: IO()
main = do
    -- Get command line arguments
    args <- getArgs
    -- Get the mode for interpreter
    Control.Monad.when (length args < 3) $ error "Must specify a mode, an output file and an input file";
    putStrLn $ modeMessage (head args)
    -- Program arguments given after mode and output file arguments
    let programArgs = drop 2 args
    -- Read the Lambad program
    program <- getProgram programArgs
    -- Run program in the indicated mode
    let lambadProgram = parse (fixWeirdEncodingError program)
    let output = run lambadProgram (head args)
    -- Save output to output file
    withFile (args !! 1) WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle (unpack output)
