module Main where

import Prelude
import Data.Int
import Data.Maybe
import Data.String
import Prelude
import Data.Generic.Rep (class Generic, to)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (appendString)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String
derive instance newTypeCSV :: Newtype CSV _
derive newtype instance showCSV :: Show CSV
derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
    toCSV :: a -> CSV
  

newtype Age = Age Int
derive instance newTypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive instance eqAge :: Eq Age

newtype FullName = FullName String
derive instance newTypeFullName :: Newtype FullName _
derive instance eqFullName :: Eq FullName
instance showFullName :: Show FullName where
  show (FullName name )= name

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance occupationGeneric :: Generic Occupation _
derive instance eqOccupation :: Eq Occupation
instance showOccupation :: Show Occupation where
   show = genericShow

data Person = Person {
    name :: FullName,
    age :: Age,
    occupation :: Occupation
}

instance toCSVPerson :: ToCSV Person where
    toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

instance showPerson :: Show Person where
   show (Person {name, age, occupation})= "(FullName: " <> show name <> " Age: " <> show age <> " Occupation: " <> show occupation <> ")"
derive instance eqPerson :: Eq Person 


class FromCSV a where
    fromCSV :: CSV -> Maybe a
  
toOccupation:: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" = Just Dentist
toOccupation "Lawyer" = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _ = Nothing

toInt:: String -> Maybe Int 
toInt str = fromString str


instance fromCSVPerson :: FromCSV Person where
    fromCSV (CSV str) = case split (Pattern ",") str of
      [name,age,occupation] -> case toInt age of 
        Just age' -> case toOccupation occupation of
           Just occupation' -> Just $ Person {name:FullName name, age: Age age', occupation: occupation'}
           Nothing -> Nothing
        Nothing -> Nothing
      _ -> Nothing

toPerson:: CSV -> Maybe Person
toPerson (CSV str) = case split (Pattern ",") str of
  [name, age, occupation] -> do
    age' <- fromString age
    occupation' <- toOccupation occupation 
    pure $ Person {name:FullName name, age: Age age', occupation: occupation'}
  _ -> Nothing

person1::Person
person1 = Person{name: FullName "Chu Thin", age: Age 34, occupation: Unemployed}

person:: Maybe Person
person = fromCSV $ toCSV $ person1

test :: Effect Unit 
test = do 
    log "123"
    log $ show $ Person{name: FullName "Chu Thin", age: Age 34, occupation: Unemployed}
    log $ show $ toCSV $ Person{name: FullName "Chu Thin", age: Age 34, occupation: Unemployed}
    log $ show $ toCSV (Person{name: FullName "Chu Thin", age: Age 34, occupation: Unemployed}) == CSV "Chu Thin,34,Unemployed"
    log $ show $ person
    log $ show $ (person == Just person1)
    log $ show $ toPerson $ CSV "Chu Thin,34"
    log $ show $ toPerson $ CSV "Chu Thin,34,Unemployed"
