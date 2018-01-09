module January4 where

import Data.List (intercalate)

-- Write a data type to represent JSON
-- Implement "Show" on it to print it. Or pretty print maybe?

newtype JObject = JObject [JRecord]
instance Show JObject where
  show (JObject jrecords) = "{" ++ showRecords ++ "}"
    where
      showRecords = intercalate "," $ map show jrecords

data JRecord = JRecord String JValue
instance Show JRecord where
  show (JRecord key jvalue) = show key ++ ":" ++ show jvalue


data JValue = JValueArray [JValue]
            | JValueObject JObject
            | JValueString String
            | JValueNumber Double
            | JValueBool Bool
            | JValueNull
instance Show JValue where
  show (JValueObject o) = show o
  show (JValueArray a) = show a
  show (JValueString s) = show s
  show (JValueNumber i) = show i
  show (JValueBool True) = "true"
  show (JValueBool False) = "false"
  show JValueNull = "null"
