module January4 where

-- Write a data type to represent JSON
-- Implement "Show" on it to print it. Or pretty print maybe?

data JObject = JObject String JValue JObject
             | JObjectEmpty
             | JObjectEnd
             deriving (Show)

data JArray = JArray JValue JArray
            | JArrayEmpty
            | JArrayEnd
            deriving (Show)

data JValue = JValueObject JObject
            | JValueArray JArray
            | JValueString String
            | JValueInt Int
            | JValueBool Bool
            | JValueNull
            deriving (Show)

data JSON = WithObject JObject
          | WithArray JArray
          | WithValue JValue
          deriving (Show)
