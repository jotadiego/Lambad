-----------------------------------------------------------------------------------------------------------------------
-- Lambad
-- https://github.com/jotadiego/Lambad
-----------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Lambad (LambadProgram (..), LambadStatement (..), showVerbose, isIdentity, parse, toLambda) where

import Control.Applicative ( Alternative(..) )
import Data.Char (ord, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Lambda

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Modelling Lambad programs
--
-- Programs with a regular return values are modelled as a context with _n_ variables, a list of statements
-- corresponding to applications or compositions, and a selector _m_ indicating which expression is to be returned.
--
-- Programs which return a composition use an alternate constructor based on the two intervening subprograms.
-- A program is modelled either as a context with _n_ variables, a list of statements (application or compositons)
data LambadProgram = LambadContext Int [LambadStatement] Int | LambadComposition LambadProgram LambadProgram

-- Two kinds of statements are considered: applications (determined by the index of the intervening expressions)
-- and compositions, determined by the subprograms to be composed.
--
-- Variable introduction is handled separatedly by the variable count in the `LambadContext` constructor.
data LambadStatement = Application Int Int | Composition LambadProgram LambadProgram

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- String representations of Lambad programs
--
-- The functions `showVerbose` and `showShort` are defined in order to represent a LambadProgram value as the
-- corresponding Verbose Lambad or Shortened Lambad expressions.

-- The GHCI interactive interpreter for Haskell has poor Unicode compatibility (shame on it).
-- In order to ensure compatibility with it, the '×' character ('times' sign) is replaced with an 'x'(lowercase X) in
-- internal representations only. Program input and output will still use the proper '×' character as defined on the
-- language specification.

-- Shortened Lambad expression as default string representation for LambadProgram values.
instance Show LambadProgram where
    show = showShort

-- Verbose Lambad representation of a LambadStatement values.
-- (The representation of individual statements in Shortened Lambad might vary depending on their context)
instance Show LambadStatement where
    show (Application n m) = show n ++ "." ++ show m ++ ";"
    show (Composition p1 p2) = "[" ++ showVerbose p1 ++ "x" ++ showVerbose p2 ++ "]"

-- Show program as Verbose Lambad:

showVerbose :: LambadProgram -> String
showVerbose (LambadContext 0 [] ret) = ":" ++ show ret;
showVerbose (LambadContext 0 (st:sts) ret) = show st ++ showVerbose (LambadContext 0 sts ret)
showVerbose (LambadContext 1 ls ret) = showVerbose (LambadContext 0 ls ret)
showVerbose (LambadContext n ls ret) = "+" ++ showVerbose (LambadContext (n - 1) ls ret)
showVerbose (LambadComposition p1 p2) = ":" ++ show (Composition p1 p2)

-- Show program as Shortened Lambad:
-- Note: there is not just one way of representing a Shortened Lambad program as the syntactic sugar introduced by this
-- variant may be freely applied (or not). For instance, Verbose Lambad expression `++:2` (equivalent to λx.λy.λz.z)
-- could be represented in Shortened Lambad as `2+:, `:2` or even as `++:2` (any Verbose Lambad code will still be
-- valid as Shortened Lambad code). The output of `showShort` is just one 'normalized' Shortened representation.

showShort :: LambadProgram -> String
showShort p@(LambadContext n ls ret) = shortVarIntroductions p ++ shortSemicolons (shortStatements (n-1) ls ret True)
showShort (LambadComposition p1 p2) = ":[" ++ showShort p1 ++ "x" ++ showShort p2 ++ "]"

-- Auxilliary functions for computing the Shortened Lambad syntax:

-- Number of variables that could automatically be inferred for a Lambad context based on its first statement or
-- return value. Undefined for LambadComposition type programs.
inferredVariables :: LambadProgram -> Int
inferredVariables (LambadContext _ [] ret) = 1 + max ret 0;
inferredVariables (LambadContext _ (st:sts) ret) = case st of Application n m -> 1 + max (max n m) 0
                                                              _ -> 1

-- Var introduction in the shortened format
shortVarIntroductions :: LambadProgram -> String
shortVarIntroductions p@(LambadContext n _ _) | n == inferredVariables p = "" -- Implicit var introduction
                                              | n == 2 = "+"                  -- Add one variable as a single `+`
                                              | otherwise = show (n-1) ++ "+" -- `{n-1}+` for _n-1_ introductions
shortVarIntroductions _ = ""

-- Check if a program corresponds to the identity expression λx.x, `:` in Shortened Lambad.
isIdentity :: LambadProgram -> Bool
isIdentity (LambadContext 1 [] 0) = True
isIdentity _ = False

lambadIdentity :: LambadProgram
lambadIdentity = LambadContext 1 [] 0

-- Show first number if not equal to the second number
sfine :: Int -> Int -> String
sfine n m = if n == m then "" else show n

-- Print the statements and return value in a Lambad program in shortened form knowing the last defined position
-- in the context list at any given point in order to allow for references to such position to be omitted.
-- A flag (fourth argument) is used to prevent argument omission in the first statement in cases where it would
-- conflict with the automatic variable introduction mechanism.
shortStatements :: Int -> [LambadStatement] -> Int -> Bool -> String
shortStatements last [] ret False = ":" ++ sfine ret last
shortStatements last [] 0 True = ":"
shortStatements last [] ret True = ":" ++ show ret
shortStatements last (st:sts) ret False = case st of Application n m   -> sfine n last ++ "." ++ sfine m last ++ ";"
                                                                          ++ shortStatements (last+1) sts ret False
                                                     Composition p1 p2 -> (
                                                                              if isIdentity p1
                                                                              then "[" ++ showShort p2 ++ "]"
                                                                              else "[" ++ showShort p1 ++ "x"
                                                                                       ++ showShort p2 ++ "]"
                                                                          ) ++ shortStatements (last+1) sts ret False
shortStatements last (st:sts) ret True = case st of Application n m -> n' ++ "." ++ m' ++ ";"
                                                                       ++ shortStatements (last+1) sts ret False
                                                                       where n' = if n == last && n == 0
                                                                                  then ""
                                                                                  else show n
                                                                             m' = if n == last && m == n
                                                                                  then ""
                                                                                  else show m
                                                    _ -> shortStatements last (st:sts) ret False

-- Remove optional semicolons (before colons or brackets)
shortSemicolons :: String -> String
shortSemicolons (';':':':xs) = ':' : shortSemicolons xs
shortSemicolons (';':'[':xs) = '[' : shortSemicolons xs
shortSemicolons (x:xs) = x : shortSemicolons xs
shortSemicolons "" = ""

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Lambad parser
-- A parser is defined for Shortened Lambad.
-- Since Shortened Lambad is a superset of Verbose Lambad, this parser will function with both variants.

-- Characters that might appear in a Lambad program; all other characters will be ignored
validSymbols :: [Char]
validSymbols = ['0'..'9'] ++ ['[', ']', '+', '.', ':', ';', '\xd7', '-']

removeInvalid :: Text -> Text
removeInvalid = T.filter (`elem` validSymbols)

-- Building a monadic parser*, because it's actually fairly convenient, scary** as it might sound.
-- * Applicative, actually, although it's also a monad*** for reasons****.
-- ** Monads are just mystified***** wrapper functions. Legend says that once you've finally understood them, you're
-- cursed to no longer being able to explain them. That's false, there are _a few_ good explanations out there, with
-- 'Functors, Applicatives, And Monads in Pictures' (link below) likely being the best of them.
-- https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- *** All monads are applicatives, although not all applicatives are monads.
-- **** Parsing alternatives can be solved easily by making our parsers an instance of the Haskell class 'Alternative',
-- which requires the type to be a monad. If not for that, making our parsers just applicatives would have been enough.
-- ***** I'm convinced a major reason why monads have such a dreadful reputation as a difficult concept has to do with
-- the usage of obscure-looking symbols like '>>='. Monads are not to blame for that kind of notation, Haskell is.
-- ****** I should really go easy with _them_ asterisks, shouldn't I?

-- Back to the actual code:

-- The parser type; actually a type constructor (* -> *) which tries to parse a string into a value of an arbitrary
-- type `a` (given a 'run parser' function with the necessary type signature).
newtype Parser a = P {runP :: Text -> [(a, Text)]}

instance Functor Parser where
    fmap f p = P $ \s -> [(f a, s1) | (a, s1) <- runP p s]

instance Applicative Parser where
  pure a = P $ \s -> [(a, s)]
  (<*>) (P pf) (P pa) = P $ \s -> [(f a, s2) | (f , s1) <- pf s, (a , s2) <- pa s1]

instance Monad Parser where
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]

pList :: Parser a -> Parser [a]
pList p = do a <- p
             as <- pList p
             return (a:as)
          <|>
          return []

instance Alternative Parser where
    empty = P $ const []
    (<|>) (P p) (P q) = P $ \s -> p s ++ q s
    some p = (:) <$> p <*> many p
    many = pList

-- Some auxilliary functions

item :: Parser Char
item = P $ \cs -> case cs of
                    "" -> []
                    t  -> [(T.head t, T.tail t)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else empty

pDigit :: Parser Int
pDigit = do c <- pSat isDigit
            return (ord c - ord '0')

pDigits :: Parser [Int]
pDigits = some pDigit

digitListToInt :: [Int] -> Int
digitListToInt = foldl (\ x y -> 10*x + y) 0

pToNum :: Parser ([Int] -> Int)
pToNum = pure $ \ns -> digitListToInt ns

pNatural :: Parser Int
pNatural = pToNum <*> pDigits

pPlus :: Parser Char
pPlus = pSat (== '+')

pPlusses :: Parser [Char]
pPlusses = some pPlus

-- Intermediate data types based on the BNF:

data PProgram = PProgram (Maybe Int) [POps] PReturn
    deriving Show

data POps = PApply (Maybe Int) (Maybe Int) | PCompose PProgram PProgram | PComposeId PProgram
    deriving Show

data PReturn = PReturnId (Maybe Int) | PReturnCompose PProgram PProgram
    deriving Show

-- Parse functions

parseProgram :: Parser PProgram
parseProgram = do vars <- parseVars
                  ops <- parseOps
                  PProgram vars ops <$> parseReturn

parseVars :: Parser (Maybe Int)
parseVars = Just <$> parseVarPlusses <|> Just <$> parseNVar <|> pure Nothing
            where parseVarPlusses = do length <$> pPlusses
                  parseNVar = do n <- pNatural
                                 pPlus
                                 return n

parseOps :: Parser [POps]
parseOps = many parseOp

parseOp :: Parser POps
parseOp = parseApply <|> parseCompose <|> parseComposeId

parseId :: Parser (Maybe Int)
parseId = Just <$> pNatural <|> Just <$> pNegative <|> pure Nothing
          where pNegative = do pSat (== '-')
                               n <- pNatural
                               return (-n)

parseApply :: Parser POps
parseApply = do id1 <- parseId
                pSat (== '.')
                id2 <- parseId
                pSat (== ';') <|> pure 'N'
                return (PApply id1 id2)

parseCompose :: Parser POps
parseCompose = do pSat (== '[')
                  p1 <- parseProgram
                  pSat (== '\xd7')
                  p2 <- parseProgram
                  pSat (== ']')
                  return (PCompose p1 p2)

parseComposeId :: Parser POps
parseComposeId = do pSat (== '[')
                    p <- parseProgram
                    pSat (== ']')
                    return (PComposeId p)

parseReturn :: Parser PReturn
parseReturn = parseReturnCompose <|> parseReturnId

parseReturnId :: Parser PReturn
parseReturnId = do pSat (== ':')
                   PReturnId <$> parseId

parseReturnCompose :: Parser PReturn
parseReturnCompose = do pSat (== ':')
                        pSat (== '[')
                        p1 <- parseProgram
                        pSat (== '\xd7')
                        p2 <- parseProgram
                        pSat (== ']')
                        return (PReturnCompose p1 p2)

-- Transform the PProgram value into a LambdaProgram

lambadden :: PProgram -> LambadProgram
lambadden (PProgram mVars ops (PReturnId mRet)) = LambadContext varCount ops' ret
                                                    where varCount = pGetVarCount mVars ops mRet
                                                          ops' = opsToStatements ops (varCount-1)
                                                          ret = pGetRetValue mRet (varCount + length ops')
lambadden (PProgram _ _ (PReturnCompose p1 p2)) = LambadComposition (lambadden p1) (lambadden p2)

pGetVarCount :: Maybe Int -> [POps] -> Maybe Int -> Int
pGetVarCount (Just n) _ _ = n + 1
pGetVarCount Nothing (op:ops) _ = case op of PApply ma mb -> if ma == Nothing && mb == Nothing
                                                             then 1
                                                             else 1 + max 0 (max (out ma) (out mb))
                                                             where out (Just n) = n
                                                                   out Nothing = 0
                                             _ -> 1
pGetVarCount Nothing [] mRet = case mRet of Just ret -> 1 + max ret 0
                                            Nothing -> 1

opsToStatements :: [POps] -> Int -> [LambadStatement]
opsToStatements [] _ = []
opsToStatements (op:ops) last = st : opsToStatements ops (last+1)
                                where st = case op of PApply ma mb -> Application (fill ma) (fill mb)
                                                      PCompose p1 p2 -> Composition (lambadden p1) (lambadden p2)
                                                      PComposeId p -> Composition lambadIdentity (lambadden p)
                                      fill (Just n) = n
                                      fill Nothing = last

pGetRetValue :: Maybe Int -> Int -> Int
pGetRetValue (Just ret) _ = ret
pGetRetValue Nothing last = last - 1

-- Finally, our String to LambadProgram parse function
parse :: Text -> LambadProgram
parse s = if snd p == "" then lambadden $ fst p else error $ "Couldn't parse string" ++ show (removeInvalid s)
          where p = head $ runP parseProgram $ removeInvalid s

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Convert Lambad program to an equivalent Lambda expression

toLambda :: LambadProgram -> Lambda
toLambda p = toLambdaWithContext p (buildContext p 0) True (-1)

-- Convert a program to a Lambda expression, with a dictionary keeping tracks of variables available in its context
-- a boolean flag to determine whether abstractions need to be added, and the last used variable index so far.
toLambdaWithContext :: LambadProgram -> Map Int Int -> Bool -> Int -> Lambda
toLambdaWithContext p@(LambadContext vars ls r) c True i =
        addAbstractions (absVarList (vars - 1) c) $ toLambdaWithContext p c False (i + vars)
toLambdaWithContext (LambadContext vars ls r) c False i =
        if r < vars
        then LVar (fromJust $ Map.lookup r c)
        else case ls !! (r - vars) of
                                        Application a b -> LApp l1 l2
                                                            where
                                                                p1 = LambadContext vars ls a
                                                                l1 = toLambdaWithContext p1 c False i
                                                                i' = maxVarId l1
                                                                p2 = LambadContext vars ls b
                                                                l2 = toLambdaWithContext p2 c False i'
                                        Composition s1 s2 -> LApp l1 l2
                                                            where
                                                                l1 = toLambdaWithContext s1 c1 True (i+1)
                                                                c1 = combineContext s1 c (i+1)
                                                                i' = 1 + maxVarId l1
                                                                l2 = toLambdaWithContext s2 c2 True i'
                                                                c2 = combineContext s2 c i'
toLambdaWithContext (LambadComposition p1 p2) _ _ i = LApp l1 l2
                                                      where
                                                          l1 = toLambdaWithContext p1 (buildContext p1 (i+1)) True i
                                                          i' = maxVarId l1
                                                          l2 = toLambdaWithContext p2 (buildContext p2 (i'+1)) True i'

-- Build a context for a Lambad program as a mapping between context expression list positions and variable indices
-- New variable indices are generated starting from a certain index (second argument)
buildContext :: LambadProgram -> Int -> Map Int Int
buildContext (LambadContext n ls r) m = Map.fromList $ map (\x -> (x, x + m)) [0..(n - 1)]
buildContext _ _ = Map.empty

-- List of variable indexes to be used in abstractions
absVarList :: Int -> Map Int Int -> [Int]
absVarList 0 c = [fromJust $ Map.lookup 0 c]
absVarList n c = fromJust (Map.lookup n c) : absVarList (n-1) c

-- Add abstractions for the context variables
addAbstractions :: [Int] -> Lambda -> Lambda
addAbstractions xs l = foldl (flip LAbs) l xs

-- Greatest position in a context
maxContextPosition :: Map Int Int -> Int
maxContextPosition = Map.foldrWithKey (\k _ acc -> max k acc) 0

-- Least (most negative) position in a context
minContextPosition :: Map Int Int -> Int
minContextPosition = Map.foldrWithKey (\k _ acc -> min k acc) 0

-- Creata a combined context for a subprogram, including variables in the outer context with negative indices
combineContext :: LambadProgram -> Map Int Int -> Int -> Map Int Int
combineContext p c m = addOuterVariables (buildContext p m) c (minContextPosition c) (maxContextPosition c) (-1)

-- Given an inner context, an outer context, a range of positions in the outer context and a header position in the
-- inner context, incorporates outer context variables to the inner context using negative positions.
addOuterVariables :: Map Int Int -> Map Int Int -> Int -> Int -> Int -> Map Int Int
addOuterVariables inner outer outMin outMax header | outMax >= 0 && (- 1 - header) <= outMax =
                                                        Map.insert header (fromJust (Map.lookup (- 1 - header) outer))
                                                        $ addOuterVariables inner outer outMin outMax (header - 1)
                                                   | outMax >= 0 =
                                                        addOuterVariables inner outer outMin (-1) header
                                                   | outMin > outMax =
                                                        inner
                                                   | otherwise =
                                                        Map.insert header (fromJust (Map.lookup outMax outer)) $
                                                        addOuterVariables inner outer outMin (outMax -1) (header - 1)
