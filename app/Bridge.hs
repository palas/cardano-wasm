{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bridge where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Script as Script
import qualified Cardano.Api.Ledger as Ledger
import Data.ByteString.UTF8 (fromString, toString)
import Data.Function ((&))
import Data.Proxy (Proxy (..))

import GHC.Wasm.Prim

import qualified Data.Aeson as Aeson

foreign import javascript unsafe "JSON.parse($1)"
    js_parse :: JSString -> IO JSVal

foreign import javascript unsafe "JSON.stringify($1)"
    js_stringify :: JSVal -> IO JSString

jsonToJSVal :: (Api.ToJSON a) => a -> IO JSVal
jsonToJSVal a = do
    js_parse (toJSString (toString (Api.serialiseToJSON a)))

jsValToJSON :: (Api.FromJSON a) => JSVal -> IO a
jsValToJSON val = do
    jsString <- js_stringify val
    let jsonString = fromJSString jsString
    let asType = typeProxy
    case either (Left . Api.JsonDecodeError) Right $ Aeson.eitherDecodeStrict' (fromString jsonString) of
        Left err -> error ("Wrong format for argument when deserialising: " ++ show err)
        Right a -> return a
  where
    typeProxy :: Proxy a
    typeProxy = Proxy

foreign export javascript "mkTxIn"
    mkTxIn :: JSString -> Int -> IO JSVal
mkTxIn txId txIx = do
    let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId (fromString (fromJSString txId))
    let srcTxIx = Api.TxIx (fromIntegral txIx)
    let txIn = Api.TxIn srcTxId srcTxIx
    jsonToJSVal txIn

foreign export javascript "makeTransaction"
    makeTransaction :: JSVal -> IO JSVal
makeTransaction txIn = do
    let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    srcTxIx <- jsValToJSON txIn
    let txIn =
            ( srcTxIx
            , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
            )
    let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra $ Api.AsConwayEra) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
    let txOut =
            Api.TxOut
                destAddress
                (Api.lovelaceToTxOutValue sbe (fromInteger (fromIntegral (10_000_000))))
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
    jsonToJSVal envelope
