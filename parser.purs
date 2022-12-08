
module Main where

import Prelude

import Data.Foldable (fold)
import Data.Tuple (Tuple)
import Data.Either (Either)
import Effect (Effect)
import Effect.Console
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

type ParserState a = Tuple String a
class ParserError (e :: Type)
type ParserFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser e a = Parser (ParserFunction e a)

instance functorParser :: Functor (Parser e) where
  map :: forall a b . (a -> b) -> Parser e a -> Parser e b
  -- map f (Parser g) = Parser ( String -> Either e (ParserState b))
  -- g ~ String -> Either e (ParserState a)
  -- g s ~ Either e (ParserState a) ~ Either e (Tuple String a)
  -- g s apply f ~ Either e ((Tuple String a ) map f) ~ Either e (Tuple String f a) ~ Either e (Tuple String b)
  {- 
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
  map f (Parser g) = Parser \s -> map (map f) (g s) -- ~ Parser \s -> map f <$> g s
main :: Effect Unit
main = do
  log "Hello World"
