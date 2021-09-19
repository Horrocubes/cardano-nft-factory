{-|
Module      : Horrocubes.MintingScript.
Description : Mint policy for NFTs.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

This policy creates an NFT and uses an UTXO to make the NFT truly unique.

This minting policy was taken from https://github.com/input-output-hk/lobster-challenge
and sightly modified to the the token name as an outside parameter.

-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
 {-# LANGUAGE OverloadedStrings #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Horrocubes.MintingScript
(
  mintScript
) where

-- IMPORTS --------------------------------------------------------------------

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)

-- DEFINITIONS ----------------------------------------------------------------


{- HLINT ignore "Avoid lambda" -}

-- | Creates the minting script for the NFT.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy tn utxo _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                            traceIfFalse "Wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

-- | Compiles the policy.
nftPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
nftPolicy utxo nftTokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' -> Scripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftTokenName
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

-- | Generates the plutus script.
nftPlutusScript :: TxOutRef -> TokenName -> Script
nftPlutusScript utxo tn = unMintingPolicyScript $ nftPolicy utxo tn

-- | Generates the NFT validator.
nftValidator :: TxOutRef -> TokenName -> Validator
nftValidator utxo tn = Validator $  nftPlutusScript utxo tn

-- | Serializes the contract in CBOR format.
nftScriptAsCbor :: TxOutRef -> TokenName -> LB.ByteString
nftScriptAsCbor utxo tn = serialise $ nftValidator utxo tn

-- | Gets a serizlize plutus script from the given UTXO and token name.
mintScript :: TxOutRef -> BuiltinByteString -> PlutusScript PlutusScriptV1
mintScript utxo tn = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftScriptAsCbor utxo $ toTokenName tn

-- | Creates a token name from a byte array.
toTokenName :: BuiltinByteString -> TokenName
toTokenName tn = TokenName { unTokenName = tn }