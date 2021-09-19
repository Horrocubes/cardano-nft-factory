{-|
Module      : plutus-horrocubes-tokens.
Description : Application to generate NFTs using smart contracts.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- IMPORTS --------------------------------------------------------------------

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)
import Data.Hex

import Horrocubes.MintingScript

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
-- The user must provide three arguments: UTXO id, the token name and the output path.
main :: IO ()
main = do
    [utxo', tokenName', filePath] <- getArgs
    let utxo      = parseUTxO utxo'
        tokenName = getLedgerBytes $ fromString $ hex tokenName'

    nftPolicyResult <- writeFileTextEnvelope filePath Nothing $ mintScript utxo tokenName
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ filePath

-- | Parse the UTXO from its hexadecimal string representation to and TxOutRef.
parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y