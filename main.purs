module Main where

import Prelude
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Foldable (fold)
import Data.Maybe
import Effect (Effect)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)
{-
singleton :: forall a. a -> List a
singleton x = Cons x Nil

listData::List Int
listData = Cons 1 (Cons 2 (Cons 3 Nil))

length::forall a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs 


snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

head' :: forall a. List a -> Maybe a
head' Nil = Nothing
head' (x: _) = Just x

last' :: forall a. List a -> Maybe a
last' Nil = Nothing
last' (x : Nil) = Just x
last' (_ : xs ) = last' xs

init' :: forall a. List a -> Maybe (List a)
init' Nil = Nothing
init' l = Just $ go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x: go xs
  
uncons' :: forall a. List a -> Maybe {head:: a, tail:: List a}
uncons' Nil = Nothing
uncons' (x: xs) = Just { head: x, tail: xs}

forIndex :: forall a. List a -> Int -> Maybe a
forIndex Nil _ = Nothing
forIndex l i = go l i where
  go Nil _ = Nothing
  go (x : xs) ci
    | ci < 0 = Nothing
    | ci == 0 = Just x
    | otherwise = go xs (ci - 1)
    
forIndex' :: forall a. List a -> Int -> Maybe a
forIndex' Nil _ = Nothing
forIndex' l i = go l i where
  go Nil _ = Nothing
  go (x : _) 0 = Just x
  go (_ : xs) y = go xs (y -1)
  
findIndex' :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex' _ Nil = Nothing
findIndex' f l = go 0 l where
  go _ Nil = Nothing
  go i (x : xs) 
    | f x = Just(i)
    | otherwise = go ( i + 1) xs
    
findLastIndex' :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex' _ Nil = Nothing
findLastIndex' f l = go Nothing 0 l where
  go li _ Nil = li
  go li i (x : xs) = go (if f x then Just(i) else li) (i + 1) xs
  
findLastIndex1 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex1 _ Nil = Nothing
findLastIndex1 f l = go Nothing 0 l where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go li _ Nil = li
  go li i (x : xs) 
    | f x = go (Just i) (i + 1) xs
    | otherwise = go li (i + 1) xs
-}
reverse' :: forall a. List a -> List a
reverse' Nil = Nil
reverse' l = go l Nil where
  go Nil r = r
  go (x : xs) r = go xs (x:r)

concat' :: forall a. List (List a) -> List a
concat' Nil = Nil
concat' (Nil:xxs) = concat' xxs
concat' ((x:xs):xxs) = x : concat' (xs: xxs)


filter' :: forall a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' f l = reverse' $ go l Nil where
  go Nil rs = rs
  go (x : xs) rs 
    | f x = go xs (x : rs)
    | otherwise = go xs rs
{-
catMaybe' :: forall a. List(Maybe a) -> List a
catMaybe' Nil = Nil
catMaybe' (x:xs) = case x of
  Just y -> y : catMaybe' xs
  Nothing -> catMaybe' xs
-}

rang':: Int -> Int -> List Int 
rang' start end = go Nil end where
    step = if start > end then 1 else -1
    go l end' 
        | end' == start = end':l
        | otherwise = go (end':l) (end' + step) 
 
take':: forall a. Int -> List a -> List a
take' t l = reverse' $ go (max t 0) l Nil where
  go 0 _ nl = nl 
  go _ Nil nl = nl
  go t (x:xs) nl = go (t - 1) xs (x:nl)
  
  
listData1::List Int
listData1 = rang' 1 (-9) 

main :: Effect Unit
main = do
    {-log $ show $ singleton "xyz"
    log $ show $ listData
    log $ show $ length listData
    log $ show $ snoc listData 4
    log $ show $ head' listData
    log $ show $ head' $ "a" : "b" : "c" : Nil
    log $ show $ last' $ "a" : "b" : "c" : Nil
    log $ show $ init' $ "a" : "b" : "c" : Nil
    log $ show $ init' (Nil:: List Unit)
    log $ show $ forIndex ("a" : "b" : "c" : Nil) (-1)
    log $ show $ forIndex' ("a" : "b" : "c" : Nil) 1
    log $ show $ findIndex' (_ >= 0) (1 : 2 : 3 : Nil) -}
    -- log $ show $ findLastIndex' (_ == 2) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
    -- log $ show $ findLastIndex1 (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
    -- log $ show $ reverse' ("a" : "b" : "c" : Nil)
    -- log $ show $ concat' ((1: 2 : Nil) : (3 : 4 : Nil):(5: Nil): Nil)
    -- log $ show $ filter' (_ >= 2) (1 : 5 : 3 : -1 : 2 : 10 : Nil)
    -- log $ show $ catMaybe' (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
    log $ show $ rang' 9 (-5)
    log $ show $ take' 5 listData1
