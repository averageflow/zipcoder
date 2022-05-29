{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module ZipCoder.Lib where

import Data.IntMap (fromList)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Options.Applicative
import System.Exit
import Text.Printf (printf)
import Text.RawString.QQ
import Text.Regex.TDFA
import ZipCoder.Model
import ZipCoder.OptionsParser

type ZipCodeValidationFunction = (String -> Bool)
type MasterZipCodeValidator = Map.Map String ZipCodeValidationFunction

verifyValidCountryCode :: String -> Bool
verifyValidCountryCode = (=~ [r|^[A-Z]{2}$|])

getMasterZipCodeValidator :: MasterZipCodeValidator
getMasterZipCodeValidator =
  Map.fromList
    [ ("JP", (=~ [r|^[0-9]{3}-[0-9]{4}$|])),
      ("NL", (=~ [r|^[0-9]{4}[A-Z]{2}$|]))
    ]

extractBadEntries :: ZipCodeValidationFunction -> [String] -> [String]
extractBadEntries f xs =
  filter (/= "") $
    map (\x -> if f x then "" else x) xs

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

app :: ZipCoderOptions -> IO ()
app options = do
  putStrLn "λ ZipCoder - Powerful zip-code validation"
  putStrLn "λ Starting ZipCoder after successful parsing of options.."

  if verifyValidCountryCode countryCode'
    then putStrLn ""
    else putStrLn "[ERROR] Invalid country code! Terminating program!" >> exitFailure

  case maybeValidationF of
    Just v -> putStrLn $ printf "λ Initiating validation for country %s..\n" (countryCode options)
    Nothing -> putStrLn (printf "[ERROR] No parser exists for the desired country: %s" countryCode') >> exitFailure

  -- if all are valid continue, otherwise create a list with all the failing ones
  if allZipCodesAreValid
    then putStrLn "λ All zip-codes are valid!" >> exitSuccess
    else do
      putStrLn "[ERROR] Detected bad data! Bad entries will be printed below:\n"
      print (extractBadEntries validationFunction zipCodeList) >> exitFailure
  where
    countryCode' = countryCode options
    maybeValidationF = Map.lookup countryCode' getMasterZipCodeValidator
    validationFunction = fromJust maybeValidationF
    zipCodeList = extractCodesFromOptions (zipCodes options)
    allZipCodesAreValid = all validationFunction zipCodeList
