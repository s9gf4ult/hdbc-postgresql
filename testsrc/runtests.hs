{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
  #-}

module Runtests where

import Control.Exception (bracket)
import Data.Int
import Data.List (genericLength)
import Database.HDBI
import Database.HDBI.PostgreSQL
import Database.HDBI.Tests
import System.Environment
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit ((@?=))
import Test.QuickCheck
import Test.QuickCheck.Assertions
import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Monadic as QM



testAffectedRows :: PostgreConnection -> [Int32] -> Property
testAffectedRows c is = QM.monadicIO $ do
  res <- QM.run $ withTransaction c $ do
    runRaw c "drop table if exists table2"
    runRaw c "create table table2 (val bigint)"
    runMany c "insert into table2(val) values (?)" $ map ((:[]) . toSql) is
    withStatement c "update table2 set val=10" $ \s -> do
      execute s ()
      pgAffectedRows s

  QM.stop $ res ?== (genericLength is)

pgTests :: PostgreConnection -> Test
pgTests c = testGroup "Auxiliary functions"
           [ testProperty "affectedRows" $ testAffectedRows c
           , testCase "Check driver name" $ hdbiDriverName c @?= "postgresql"
           , testCase "Check transaction support" $ dbTransactionSupport c @?= True
           ]


fields :: TestFieldTypes
fields = TestFieldTypes
         { tfDecimal = "decimal(600,300)"
         , tfInteger = "decimal(300,0)"
         , tfDouble = "double precision"
         , tfText = "text"
         , tfBlob = "bytea"
         , tfBool = "boolean"
         , tfBitField = "varbit"
         , tfUUID = "uuid"
         , tfUTCTime = "timestamp with time zone"
         , tfLocalDate = "date"
         , tfLocalTimeOfDay = "time"
         , tfLocalTime = "timestamp without time zone"
         }


main :: IO ()
main = do
  a <- getArgs
  case a of
    (conn:args) -> bracket
                   (connectPostgreSQL $ TL.pack conn)
                   disconnect
                   $ \c -> (flip defaultMainWithArgs) args [ allTests fields c
                                                           ,pgTests c
                                                           ]

    _ -> do
      mapM_ putStrLn [ "Need at least one argument as connection string"
                     , "the rest will be passed as arguments to test-framework"]
      exitWith $ ExitFailure 1
