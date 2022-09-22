module Main where

import Prelude
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Foldable (fold)
import Data.Maybe
import Effect (Effect)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

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
    --log $ show $ findLastIndex' (_ == 2) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
    log $ show $ findLastIndex1 (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
