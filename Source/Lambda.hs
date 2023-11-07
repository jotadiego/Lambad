-----------------------------------------------------------------------------------------------------------------------
-- Lambad
-- https://github.com/jotadiego/Lambad
-----------------------------------------------------------------------------------------------------------------------

-- Modelling Lambda (λ) expressions.
{-# LANGUAGE OverloadedStrings #-}

module Lambda (Lambda (..), maxVarId, canReduce, reduce, maxReduce, safeReduce, normalize, size,
                            isBoolean, toBoolean, lTrue, lFalse,
                            isNatural, toNatural, lZero, lSucc,
                            isText, toText, toByteList, checkLambdaFormat) where

import Data.List
import Data.Maybe
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import Data.ByteString (pack)

-- Instead of variable names, variables will be identified by a number within the data structure.

data Lambda = LVar Int | LAbs Int Lambda | LApp Lambda Lambda

-----------------------------------------------------------------------------------------------------------------------

-- Convert data type values to string.
-- No attempt is done to remove unnecessary parentheses, so results will show "λx.((x x) x)"" instead of the more
-- standard (though somewhat less intuitive) "λ.(x x x)".

instance Show Lambda where
    show l = showLambda l (varNames l)

-- Largest identifier
maxVarId :: Lambda -> Int
maxVarId (LVar n) = n
maxVarId (LAbs n l) = max n (maxVarId l)
maxVarId (LApp l1 l2) = max (maxVarId l1) (maxVarId l2)

-- Show identifiers as x y z s t u v w p q r if there are up to 11 variables; otherwise the format x{n} will be used.
varNames l = if maxVarId l > 10 then map (\n -> 'x' : show n) [0..]
                                else ["x", "y", "z", "s", "t", "u", "v", "w", "p", "q", "r"]

-- Again, due to GHCI poor Unicode support, the 'lambda' character is replaced with an L in internal representations.

showLambda :: Lambda -> [String] -> String
showLambda (LVar n) ids = ids !! n
showLambda (LAbs n l) ids = 'L' : (ids !! n) ++ "." ++ showLambda l ids
showLambda (LApp l1 l2) ids = '(' : showLambda l1 ids ++ " " ++ showLambda l2 ids ++ ")"

-----------------------------------------------------------------------------------------------------------------------

-- Normalize identifiers: use minimal values for (unique) identifiers in order of apparition

-- Obtain a list without duplicates of the variables currently in the expression
varIdSequence :: Lambda -> [Int] -> [Int]
varIdSequence (LVar n) p = if elem n p then p else p ++ [n]
varIdSequence (LAbs n l) p = varIdSequence l p'
                             where p' = if elem n p then p else p ++ [n]
varIdSequence (LApp l1 l2) p = varIdSequence l2 $ varIdSequence l1 p

-- Obtain the index of the variables in the list
seqIndex :: Int -> [Int] -> Int
seqIndex n s = fromJust $ elemIndex n s

-- Replace variables with their index value
lambdaTranslate :: Lambda -> [Int] -> Lambda
lambdaTranslate (LVar n) s = LVar (seqIndex n s)
lambdaTranslate (LAbs n l) s = LAbs (seqIndex n s) (lambdaTranslate l s)
lambdaTranslate (LApp l1 l2) s = LApp (lambdaTranslate l1 s) (lambdaTranslate l2 s)

normalize :: Lambda -> Lambda
normalize l = lambdaTranslate l (varIdSequence l [])

-- Defining equality by comparing normalized strings is inefficient and lazy. That is, the 'bad' kind of lazy,
-- as opposed to the useful kind of 'lazy' as used in functional programming.
-- Let's gloss that over, though.
instance Eq Lambda where
    (==) l1 l2 = show (normalize l1) == show (normalize l2)

-----------------------------------------------------------------------------------------------------------------------

-- Beta reduction

-- Indicates whether a beta reduction may be performed
canReduce :: Lambda -> Bool
canReduce (LVar _) = False
canReduce (LAbs _ l) = canReduce l
canReduce (LApp l1 l2) = case l1 of LAbs _ _ -> True
                                    _ -> canReduce l1 || canReduce l2

-- Reduce function
reduce :: Lambda -> Lambda
reduce l = normalize $ betaReduce l (maxVarId l) []

-- Reduce as much as possible (might result in a loop)
maxReduce :: Lambda -> Lambda
maxReduce l = if canReduce l then maxReduce (reduce l) else l

-- Reduce until any of the following criteria are met:
-- a) The expression can no longer be reduced
-- b) The expression was reduced 10 000 times
-- c) The resulting expression has over 10 000 terms
-- If reduction is stopped with crtieria (b) or (c), a `Nothing` value is returned.
safeReduce :: Lambda -> Maybe Lambda
safeReduce l = reduceUntil l 10000 10000

-- Reduce a expression until it exceeds a certain size or number of steps
reduceUntil :: Lambda -> Int -> Int -> Maybe Lambda
reduceUntil l maxSize steps | steps == 0 || size l > maxSize = Nothing
                            | canReduce l = reduceUntil (reduce l) maxSize (steps - 1)
                            | otherwise = Just l

-- Number of terms in a lambda expression
size :: Lambda -> Int
size (LVar _) = 1
size (LAbs _ l) = 1 + size l
size (LApp l1 l2) = size l1 + size l2

-- Performs the first available beta-reduction
-- Additional arguments are used for tracking the last largest identifier so far and a current list of bound variables
-- in order to allow for easier alpha conversion.
betaReduce :: Lambda -> Int -> [Int] -> Lambda
betaReduce (LVar n) mv bound = LVar n
betaReduce (LAbs n l) mv bound = LAbs n (betaReduce l mv (n: bound))
betaReduce (LApp l1 l2) mv bound = case l1 of LAbs x e -> substituteB
                                                          x e (upAlphaConvert l2 (mv + 2) bound) mv
                                              _ -> if canReduce l1
                                                   then LApp (betaReduce l1 mv bound) l2
                                                   else LApp l1 (betaReduce l2 mv bound)

-- Replace (α-convert) free variables and new bound variables in an expression
-- Arguments: the lambda expression to conver
--            an index to be used for the next variable
--            a list of bound variables
upAlphaConvert :: Lambda -> Int -> [Int] -> Lambda
upAlphaConvert (LVar n) i bound | elem n bound = LVar n
                                | otherwise = LVar (n+i)
upAlphaConvert (LAbs n l) i bound = LAbs (n+i) (upAlphaConvert l i bound)
upAlphaConvert (LApp l1 l2) i bound = LApp (f l1) (f l2)
                                      where f l = upAlphaConvert l i bound

-- Substitute a variable x in an expression e with a lambda term l
-- It is assumed that l was already alpha-converted to prevent variable capture
substituteB :: Int -> Lambda -> Lambda -> Int -> Lambda
substituteB x (LVar n) l mv = if n == x then l else LVar n
substituteB x (LAbs n e') l mv = if n == x
                                 then LAbs (n + 2 * mv) (substituteB x (substituteAll x (n + 2 * mv) e') l mv)
                                 else LAbs n (substituteB x e' l mv)
substituteB x (LApp e1 e2) l mv = LApp (f e1) (f e2)
                                  where f e = substituteB x e l mv

-- Substitutes all occurrences of a variable
substituteAll :: Int -> Int -> Lambda -> Lambda
substituteAll x y (LVar n) = if n == x then LVar y else LVar n
substituteAll x y (LAbs n e') = LAbs n' (substituteAll x y e')
                                where n' = if n == x then y else n
substituteAll x y (LApp e1 e2) = LApp (f e1) (f e2)
                                where f e = substituteAll x y e

-----------------------------------------------------------------------------------------------------------------------

-- Boolean representation

lFalse :: Lambda
lFalse = LAbs 0 (LAbs 1 (LVar 0))

lTrue :: Lambda
lTrue = LAbs 0 (LAbs 1 (LVar 1))

isBoolean :: Lambda -> Bool
isBoolean l = l == lFalse || l == lTrue

toBoolean :: Lambda -> Maybe Bool
toBoolean l | l == lFalse = Just False
            | l == lTrue = Just True
            | otherwise = Nothing

-----------------------------------------------------------------------------------------------------------------------

-- Natural representation

-- Zero value
lZero :: Lambda
lZero = lFalse

-- Expression for successors
lSucc :: Lambda -- λn.λz.λs.((n (s z)) s)
lSucc = LAbs 0 (LAbs 1 (LAbs 2 (LApp (LApp (LVar 0) (LApp (LVar 2) (LVar 1))) (LVar 2))))

-- Count abstractions at the beginning of a lambda expression and return the first non-abstraction term
abstractionsAtBeginning :: Lambda -> (Int, Lambda)
abstractionsAtBeginning (LAbs _ e) = (1 + count, e')
                                     where (count, e') = abstractionsAtBeginning e
abstractionsAtBeginning e = (0, e)

-- Checks whether a Lambda expression consists of iterated applications to the left of a certain term,
-- as in λz.λs.(s (s (s (s z)))). The expected right-side terminal is indicated as the second argument.
leftSideApplications :: Lambda -> Int -> (Bool, Int)
leftSideApplications (LApp l1 l2) t = case l1 of LVar n -> if n /= t then (b, count + 1)
                                                                     else (False, 0)
                                                                     where (b, count) = leftSideApplications l2 t
leftSideApplications (LVar n) t = (n == t, 0)
leftSideApplications _ _ = (False, 0)

-- Check whether a lambda expression has a format comprised of n Abstractions followed by a series of left-side
-- applications with a certain terminal variable at the end.
checkLambdaFormat :: Int -> Int -> Lambda -> Bool
checkLambdaFormat vars terminal l = if n == vars then fst (leftSideApplications l' terminal) else False
                                    where (n, l') = abstractionsAtBeginning l

-- Check if a lambda expression has a valid format to represent a natural number
isNatural :: Lambda -> Bool
isNatural = checkLambdaFormat 2 0

toNatural :: Lambda -> Maybe Int
toNatural l = if isNatural l then Just (snd (leftSideApplications l' 0)) else Nothing
              where (n, l') = abstractionsAtBeginning l


-----------------------------------------------------------------------------------------------------------------------

-- String representation as a sequence of bytes that will hopefully encode text in UTF-8.

-- I guess nothing is stopping you from using this to encode text using other standards or character sets.
-- As a UTF-8 fan, I wouldn't like it, but I won't do anything about it.
-- After all, if you're using this, I've already hurt you enough.

isText :: Lambda -> Bool
isText = checkLambdaFormat 257 256

toByteList :: Lambda -> Maybe [Int]
toByteList l = if isText l
               then Just (reverse $ extractByteList l)
               else Nothing

extractByteList :: Lambda -> [Int]
extractByteList (LAbs _ l) = extractByteList l
extractByteList (LApp l1 l2) = case l1 of (LVar n) -> n : extractByteList l2
                                          _ -> []
extractByteList _ = []

toText :: Lambda -> Maybe T.Text
toText l = case toByteList l of Nothing -> Nothing
                                Just bs -> Just (E.decodeUtf8 $ pack $ map fromIntegral bs)