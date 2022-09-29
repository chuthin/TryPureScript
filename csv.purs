module Main where

import Prelude

import Data.Foldable (fold)
import Data.Newtype (class Newtype)
import Data.Generic.Rep
import Data.Show.Generic
import Data.Maybe
import Data.Int
import Data.String
import Effect (Effect)
import Effect.Console (log)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

newtype FullName = FullName String
derive instance newTypeFullName:: Newtype FullName _
derive instance eqFullName :: Eq FullName
instance showFullName :: Show FullName where
  show (FullName str)= str
  
newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive instance generictAge :: Generic Age _
derive instance eqAge :: Eq Age
instance showAge :: Show Age where
  show = genericShow

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _
derive instance eqOccupation :: Eq Occupation
instance showOccupation :: Show Occupation where
  show = genericShow
  
data Person = Person {
   name :: FullName,
   age:: Age,
   occupation:: Occupation 
}

instance showPerson :: Show Person where
  show (Person { name,age,occupation}) = "( name: " <> show name <> " age: " <> show  age <> " occupation: " <> show  occupation <> ")"
derive instance eqPerson :: Eq Person


newtype CSV = CSV String
derive instance newTypeCSV :: Newtype CSV _
derive instance genericCSV :: Generic CSV _
derive instance eqCSV :: Eq CSV
instance showCSV :: Show CSV where
  show = genericShow
  
class ToCSV a where
  toCSV :: a -> CSV

instance toCSVPerson :: ToCSV Person where
  toCSV (Person{name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation
  
class FromCSV a where
  fromCSV::CSV -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" = Just Dentist
toOccupation "Lawyer" = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _ = Nothing
  
instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person{name:FullName name, age: Age age', occupation: occupation'}
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

person = Person{ name: FullName "Sue Smith" , age: Age 23, occupation: Doctor}

main :: Effect Unit
main = do
  log "demo"
  log $ show $ Person{name:FullName "Chu Thin", age:Age 34, occupation: Doctor}
  log $ show $ toCSV $ Person{name:FullName "Chu Thin", age:Age 34, occupation: Doctor}
  log $ show $ toCSV (Person{name:FullName "Chu Thin", age:Age 34, occupation: Doctor}) == CSV  "Chu Thin,(Age 34),Doctor"
  log $ show $ (toCSV person # fromCSV) == Just person
