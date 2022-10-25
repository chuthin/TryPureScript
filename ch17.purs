module Main where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import Data.Newtype (class Newtype)
import Data.Generic.Rep
import Data.Show.Generic
import Data.Functor
import Data.Bifunctor
import Data.Semigroup
import Control.Apply
import Control.Applicative
import Effect.Console (log)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
derive instance eqMaybe ::Eq a => Eq (Maybe a)
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map f (Just x) = Just $ f x
  map f Nothing = Nothing

instance applyMaybe :: Apply Maybe where
  -- apply :: forall a b. Apply f => Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMabye :: Applicative Maybe where
  pure = Just 

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left a) = Left a
  map f (Right b) = Right $ f b
  
instance biFunctor :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left a) _ = Left a

instance applycativeEither :: Applicative (Either a) where
  pure = Right
  
fullName :: String -> String -> String -> String
fullName firstName midName lastName = firstName <> " " <> midName <> " " <> lastName

errorIfMissing :: Maybe String -> String -> Either String String
errorIfMissing Nothing error = Left error
errorIfMissing (Just x) _ = Right x

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither firstName midName lastName = fullName <$> errorIfMissing firstName "Fisrt name must exist"
                                                     <*> errorIfMissing midName "Mid name must exist"
                                                     <*> errorIfMissing lastName "Last name must exist"

newtype Validation err result = Validation (Either err result)
derive instance newtypeValidation :: Newtype (Validation err result) _
derive newtype instance functorValidation :: Functor (Validation err)
derive newtype instance bifunctionValidation :: Bifunctor Validation
derive newtype instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)
derive newtype instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)
derive instance genericValidation :: Generic (Validation err result) _
derive newtype instance showValidation :: (Show err, Show result) => Show (Validation err result) 

instance applyValidation:: Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2)
  apply (Validation (Left err)) _ = Validation $ Left err
  apply (Validation (Right f)) x = f <$> x

errorIfMissing' :: Maybe String -> String -> Validation (Array String) String
errorIfMissing' Nothing error = Validation $ Left [error]
errorIfMissing' (Just x) _ = Validation $ Right x

fullNameEither' :: Maybe String -> Maybe String -> Maybe String -> Validation (Array String) String
fullNameEither' firstName midName lastName = fullName <$> errorIfMissing' firstName "Fisrt name must exist"
                                                     <*> errorIfMissing' midName "Mid name must exist"
                                                     <*> errorIfMissing' lastName "Last name must exist"

main :: Effect Unit
main = do 
  log "demo"
  log $ show $ Just 10
  log $ show $ Just 10 == Just 20
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 3 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 33
  log $ show $ (Left 22 :: (Either Int String))
  log $ show $ ((+) <$> pure 1 <*> (pure identity <*> pure 2 ):: Either Unit Int)
  log $ show $ fullNameEither (Just "Nguyen") (Just "Van") (Just "A")
  log $ show $ fullNameEither Nothing Nothing (Just "A")
  log $ show $ fullNameEither' Nothing Nothing (Just "A")
  log $ show $ fullNameEither' (Just "Nguyen") (Just "Van") (Just "A")
