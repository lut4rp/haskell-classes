module January4 where

-- Write a data type to represent JSON
-- Implement "Show" on it to print it. Or pretty print maybe?

newtype JObject = JObject [JRecord]
instance Show JObject where
  show (JObject jrecords) = "{" ++ show jrecords ++ "}"


newtype JArray = JArray [JValue]
instance Show JArray where
  show (JArray jvalues) = show jvalues


data JRecord = JRecord String JValue
instance Show JRecord where
  show (JRecord key jvalue) = show key ++ ":" ++ show jvalue


data JValue = JValueArray JArray
            | JValueObject JObject
            | JValueString String
            | JValueInt Int
            | JValueBool Bool
            | JValueNull
instance Show JValue where
  show (JValueObject o) = show o
  show (JValueArray a) = show a
  show (JValueString s) = show s
  show (JValueInt i) = show i
  show (JValueBool b) = show b
  show JValueNull = "null"
