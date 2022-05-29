module ZipCoder.OptionsParser where

import Options.Applicative
import Data.Semigroup ((<>))
import ZipCoder.Model
import Data.List.Split

extractCodesFromOptions :: String -> [String]
extractCodesFromOptions = splitOn ":"

zipCoderOptions :: Parser ZipCoderOptions
zipCoderOptions = ZipCoderOptions
    <$> strOption 
        ( long "country-code"
        <> metavar "COUNTRYCODE"
        <> help "2-letter country code of the zipcodes")
    <*> strOption 
        ( long "zip-codes"
        <> metavar "ZIPCODES"
        <> help "Semicolon(:) separated list of zipcodes to validate")

--   , quiet      :: Bool
--   , enthusiasm :: Int
--   <$> strOption
--       ( long "hello"
--      <> metavar "TARGET"
--      <> help "Target for the greeting" )
--   <*> switch
--       ( long "quiet"
--      <> short 'q'
--      <> help "Whether to be quiet" )
--   <*> option auto
--       ( long "enthusiasm"
--      <> help "How enthusiastically to greet"
--      <> showDefault
--      <> value 1
--      <> metavar "INT" )