-- | Results in latin1, as after @SET NAMES latin1@. Their bytes used to be
-- decoded as UTF-8, which threw on anything outside ASCII.
module Latin1Results (tests) where

import           Control.Monad       (void)
import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: QueryApi -> TestTree
tests api = testCase "latin1 results decode to the characters the server means" $
    withTestConnection $ \c -> do
        void (execute_ c "CREATE TEMPORARY TABLE latin1_results (b VARCHAR(200) CHARACTER SET latin1)")
        -- Every byte from 0x80 up, the part of latin1 that is not ASCII.
        void (execute_ c "INSERT INTO latin1_results VALUES (CONVERT(UNHEX(CONCAT(\
                         \'808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F',\
                         \'A0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF',\
                         \'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF',\
                         \'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF')) USING latin1))")
        serverUtf8 <- selectAll c
        void (execute_ c "SET NAMES latin1")
        (_, latin1Rows) <- apiQuery_ api (ColumnCount 1) c "SELECT b FROM latin1_results"
        textRows <- Stream.toList latin1Rows
        assertEqual "text protocol" serverUtf8 textRows
        stmt <- prepareStmt c "SELECT b FROM latin1_results"
        (_, rows) <- apiQueryStmt api (ColumnCount 1) c stmt []
        binaryRows <- Stream.toList rows
        assertEqual "binary protocol" serverUtf8 binaryRows

selectAll :: MySQLConn -> IO [[MySQLValue]]
selectAll c = do
    (_, rows) <- query_ c "SELECT b FROM latin1_results"
    Stream.toList rows
