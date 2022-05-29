{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module ZipCoder.Lib where

import Data.IntMap (fromList)
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Options.Applicative
import System.Exit
import Text.Printf (printf)
import Text.RawString.QQ
import Text.Regex.TDFA
import ZipCoder.Model
import ZipCoder.OptionsParser (zipCoderOptions)

type MasterZipCodeValidator = Map.Map String (String -> Bool)

verifyValidCountryCode :: String -> IO ()
verifyValidCountryCode code
  | isValidCountryCode = putStrLn ""
  | otherwise = putStrLn "[ERROR] Invalid country code! Terminating program!" >> exitFailure
  where
    countryCodeMatchesRegex = (code =~ [r|^[A-Z]{2}$|]) :: Bool
    isValidCountryCode = countryCodeMatchesRegex

runApplication :: IO ()
runApplication = app =<< execParser opts
  where
    opts =
      info
        (zipCoderOptions <**> helper)
        ( fullDesc
            <> progDesc "Easy, fast and powerful zipcode validation, written in Haskell"
            <> header "λ ZipCoder - Powerful zipcode validation"
        )

extractCodesFromOptions :: String -> [String]
extractCodesFromOptions = splitOn ":"

getMasterZipCodeValidator :: Map.Map String (String -> Bool)
getMasterZipCodeValidator =
  Map.fromList
    [ ("JP", (=~ [r|^[0-9]{3}-[0-9]{4}$|])),
      ("NL", (=~ [r|^[0-9]{4}[A-Z]{2}$|]))
    ]

app :: ZipCoderOptions -> IO ()
app options = do
  putStrLn "λ ZipCoder - Powerful zip-code validation"
  putStrLn "λ Starting ZipCoder after successful parsing of options.."

  verifyValidCountryCode countryCode'

  case maybeValidationF of
    Just v -> putStrLn $ printf "λ Initiating validation for country %s..\n" (countryCode options)
    Nothing -> putStrLn (printf "[ERROR] No parser exists for the desired country: %s" countryCode') >> exitFailure

  -- if all are valid continue, otherwise create a list with all the failing ones
  if zipCodeValidity
    then putStrLn "λ All zip-codes are valid!" >> exitSuccess
    else do
      putStrLn "[ERROR] Detected bad data! Bad entries will be printed below:\n"
      print (extractBadEntries validationF codes) >> exitFailure
  where
    countryCode' = countryCode options
    maybeValidationF = Map.lookup countryCode' getMasterZipCodeValidator
    validationF = fromJust maybeValidationF
    codes = extractCodesFromOptions (zipCodes options)
    zipCodeValidity = all validationF codes

extractBadEntries :: (String -> Bool) -> [String] -> [String]
extractBadEntries f xs =
  filter (/= "") $
    map (\x -> if f x then "" else x) xs