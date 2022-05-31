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
    [ ("AT", (=~ [r|^[1-9]\d{3}$|])),
      ("AU", (=~ [r|^\d{4}$|])),
      ("BE", (=~ [r|^[1-9]\d{3}$|])),
      ("CA", (=~ [r|^(?i)([^DFIOQUWZ\d]{1}[0-9]{1}[^DFIOQU\d]{1})\s*([0-9]{1}[^DFIOQU\d]{1}[0-9]{1})$|])),
      ("CH", (=~ [r|^[1-9]\d{3}$|])),
      ("CZ", (=~ [r|^[1-7]\d{4}$|])),
      ("DE", (=~ [r|^\d{5}$|])),
      ("DK", (=~ [r|^[1-9]\d{3}$|])),
      ("ES", (=~ [r|^\d{5}$|])),
      ("FI", (=~ [r|^\d{5}$|])),
      ("FR", (=~ [r|^\d{5}$|])),
      ("GB", (=~ [r|^(?i)[A-Z]{1,2}([0-9]{1,2})\s*([0-9A-Z]{3,4})$|])),
      ("HR", (=~ [r|^[1-9]\d{4}$|])),
      ("HU", (=~ [r|^[1-9]\d{3}$|])),
      ("IE", (=~ [r|^(?i)[A-Z]{1}[0-9]{1,2}[0-9W]{1}[ \-]?[0-9A-Z]{4}$|])),
      ("IT", (=~ [r|^\d{5}$|])),
      ("JP", (=~ [r|^[0-9]{3}-[0-9]{4}$|])),
      ("KR", (=~ [r|^\d{4}$|])),
      ("NL", (=~ [r|^(?i)[1-9][0-9]{3} ?(?!SA|SD|SS)[A-Z]{2}$|])),
      ("NO", (=~ [r|^\d{4}$|])),
      ("PL", (=~ [r|^\d{2}-\d{3}$|])),
      ("PT", (=~ [r|^[1-9]\d{3}-\d{3}$|])),
      ("RO", (=~ [r|^\d{6}$|])),
      ("RS", (=~ [r|^[1-9]\d{4}$|])),
      ("RU", (=~ [r|^[1-9]\d{5}$|])),
      ("SE", (=~ [r|^[1-9]\d{4}$|])),
      ("SI", (=~ [r|^[1-9]\d{3}$|])),
      ("SK", (=~ [r|^[890]\d{4}$|])),
      ("US", (=~ [r|^\d{5}$|]))
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
