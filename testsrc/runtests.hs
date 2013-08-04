{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
  #-}

module Runtests where

import Control.Exception (bracket)
import Database.HDBI
import Database.HDBI.PostgreSQL
import Database.HDBI.Tests
import System.Environment
import System.Exit
import Test.Framework
import qualified Data.Text.Lazy as TL
  
  

-- testAffectedRows :: PostgreConnection -> [Int32] -> Property
-- testAffectedRows c is = QM.monadicIO $ do
--   res <- QM.run $ withTransaction c $ do
--     runRaw c "drop table if exists table1"
--     runRaw c "create table table1 (val bigint)"
--     runMany c "insert into table1(val) values (?)" $ map ((:[]) . toSql) is

--     s2 <- prepare c "update table1 set val=10"
--     executeRaw s2
--     res <- affectedRows s2
--     finish s2
  
--     return res
--   QM.stop $ res ?== (genericLength is)
           
-- testG2 :: PostgreConnection -> Test
-- testG2 c = testGroup "Auxiliary functions"
--            [ testProperty "affectedRows" $ testAffectedRows c ]

fields = TestFieldTypes
         { tfDecimal = "decimal(600,300)"
         , tfInteger = "decimal(300,0)"
         , tfDouble = "double precision"
         , tfText = "text"
         , tfBlob = "bytea"
         , tfBool = "boolean"
         , tfBitField = "varbit"
         , tfUUID = "uuid"
         , tfUTCTime = "timestamp"
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
                   $ \c -> (flip defaultMainWithArgs) args [ allTests fields c ]

    _ -> do
      mapM_ putStrLn [ "Need at least one argument as connection string"
                     , "the rest will be passed as arguments to test-framework"]
      exitWith $ ExitFailure 1
