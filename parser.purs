module Main where

import Prelude

import Data.Foldable (fold)
import Data.Tuple 
import Data.Either 
import Effect (Effect)
import Effect.Console
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

type ParserState a = Tuple String a
class ParserError (e :: Type)
type ParserFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser e a = Parser (ParserFunction e a)

instance functorParser :: Functor (Parser e) where
  map :: forall a b . (a -> b) -> Parser e a -> Parser e b
  map f (Parser g) = Parser \s -> map f <$> g s
  
  {- 
  -- map f (Parser g) = Parser ( String -> Either e (ParserState b))
  -- g ~ String -> Either e (ParserState a)
  -- g s ~ Either e (ParserState a) ~ Either e (Tuple String a)
  -- g s apply f ~ Either e ((Tuple String a ) map f) ~ Either e (Tuple String f a) ~ Either e (Tuple String b)
    map :: forall a b. (a -> b) -> fa -> fb
    
    map g :: forall a b. fa -> fb  -- g :: a -> b
    
    g s ~ Either e (Tuple String a)
    map (map f) (g s)
    ~ map (map f) (Either e (Tuple String a))
    ~ Either e (Tuple String a map f)
    ~ Either e (Tuple String f a)
    ~ Either e (Tuple String b)
    ---
  -}
  -- map f (Parser g) = Parser \s -> map (map f) (g s) 

instance applyParser :: Apply (Parser e) where
  apply :: forall a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply (Parser f) (Parser g) = Parser \s -> case f s of
    Left err -> Left err
    Right (Tuple s1 h) -> case g s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)
  -- apply
  {-
   Parser f = Parser e (a -> b)
   Parser g = Parser e a
   f :: String -> Either e (ParserState (a -> b))
   g :: String -> Either e (ParserState a)
   g s :: Either e (ParserState a) case
      Left e -> Left e
      Right (Tuple s' a) -> (f s') ~ Either e (Tuple e b) 
   
  -}
 
main :: Effect Unit
main = do
  log "Hello World"
