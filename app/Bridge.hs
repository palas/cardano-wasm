{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bridge where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Script as Script
import qualified Cardano.Api.Ledger as Ledger
import Data.ByteString.UTF8 (toString)
import Data.Function ((&))

import GHC.Wasm.Prim

foreign import javascript unsafe "JSON.parse($1)"
    js_parse :: JSString -> IO JSVal

foreign import javascript unsafe "JSON.stringify($1)"
    js_stringify :: JSVal -> IO JSString

foreign export javascript "add42"
    add42 :: Int -> IO Int
add42 x = return (x + 42)

foreign export javascript "sayNum"
    sayNum :: Int -> IO JSString
sayNum x = return (toJSString ("The number is: " ++ show x ++ "!"))

foreign export javascript "makeTransaction"
    makeTransaction :: Int -> IO JSVal
makeTransaction val = do
    let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
    let srcTxIx = Api.TxIx 0
    let txIn =
            ( Api.TxIn srcTxId srcTxIx
            , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
            )
    let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra $ Api.AsConwayEra) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
    let txOut =
            Api.TxOut
                destAddress
                (Api.lovelaceToTxOutValue sbe (fromInteger (fromIntegral (val * 1_000_000))))
                Api.TxOutDatumNone
                Script.ReferenceScriptNone
    let txFee = Api.TxFeeExplicit sbe 2_000_000

    let txBodyContent =
            Api.defaultTxBodyContent sbe
                & Api.setTxIns [txIn]
                & Api.setTxOuts [txOut]
                & Api.setTxFee txFee
    let (Right txBody) = Api.createTransactionBody sbe txBodyContent
    let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
    let witness = Api.WitnessPaymentKey signingKey
    let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [witness]
    let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") oldApiSignedTx
    let byteString = Api.serialiseToJSON envelope
    js_parse (toJSString (toString byteString))
