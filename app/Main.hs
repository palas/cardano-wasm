{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

--import Data.Function ((&))
import qualified Cardano.Api as Api
--import qualified Cardano.Api.Ledger as Ledger
--import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Script as Script

main :: IO ()
main = do
  let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
  let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
  let srcTxIx = Api.TxIx 0
  let txIn = ( Api.TxIn srcTxId srcTxIx
             , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
             )
  let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra $ Api.AsConwayEra) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
  let txOut = Api.TxOut
                destAddress
                (Api.lovelaceToTxOutValue sbe 10_000_000)
                Api.TxOutDatumNone
                Script.ReferenceScriptNone
  let txFee = Api.TxFeeExplicit sbe 2_000_000
  print txFee
-- The following code doesn't link:
--
-- wasm-ld: error: ~/.ghc-wasm/.cabal/store/ghc-9.10.1.20250327-inplace/crdn-crypt-prs-2.2.1.0-b6ea296a/lib/libHScrdn-crypt-prs-2.2.1.0-b6ea296a.a(Praos.o): undefined symbol: crypto_vrf_ietfdraft03_seedbytes

--
--
--  let txBodyContent = Api.defaultTxBodyContent sbe
--                    & Api.setTxIns [txIn]
--                    & Api.setTxOuts [txOut]
--                    & Api.setTxFee txFee
--print txBodyContent
-- let (Right txBody) = Api.createTransactionBody sbe txBodyContent
-- print txBody
-- let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
-- let witness = Api.WitnessPaymentKey signingKey
-- let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [witness]
-- print oldApiSignedTx

