{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Control.Applicative      (liftA2)
import           Data.Bifunctor           (first)
import           Data.Monoid              (Last (..), (<>))

import           Level06.Types            (Conf (..), ConfigError (..),
                                           DBFilePath (DBFilePath),
                                           PartialConf (..), Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf :: PartialConf
defaultConf = PartialConf (return $ Port 8080) (return $ DBFilePath "db.sqlite")

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig (PartialConf (Last p) (Last f)) = case liftA2 Conf p f of
                                               Nothing -> Left IncompleteConfig
                                               Just c  -> Right c

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions :: FilePath -> IO (Either ConfigError Conf)
parseOptions f = do
  f1 <- commandLineParser
  f2 <- parseJSONConfigFile f
  case f2 of
    Left err -> return $ Left err
    Right f2' -> return $ makeConfig (defaultConf <> f2' <> f1)
