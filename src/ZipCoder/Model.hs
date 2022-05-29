module ZipCoder.Model where

data ZipCoderOptions = ZipCoderOptions
  { countryCode :: String,
    zipCodes :: String
  }deriving (Show, Eq)