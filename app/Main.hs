module Main (add42, sayNum, main) where

-- import Data.Function ((&))
-- import qualified Cardano.Api as Api
-- import qualified Cardano.Api.Ledger as Ledger
-- import qualified Cardano.Api.Experimental as Exp
-- import qualified Cardano.Api.Internal.Script as Script

-- import Foreign.C.String (CString, newCString)
-- import Foreign.Marshal.Alloc (free)

import GHC.Wasm.Prim
-- foreign import javascript unsafe "const mem = new Uint8Array(instance.exports.memory.buffer);      let end = ptr; while (mem[end] !== 0) end++; const slice = mem.slice(ptr, end); const str = utf8Decoder.decode(slice); return str;"
foreign import javascript unsafe "return \"\";"
  js_makeString :: Int -> IO JSVal

--toJSVal :: String -> IO JSVal
--toJSVal str = do
--  cstr <- newCString str
--  jstr <- js_makeString 3
--  free cstr
--  return jstr

foreign export javascript "add42"
    add42 :: Int -> IO Int
add42 x = return (x + 42)

foreign export javascript "sayNum"
    sayNum :: Int -> IO JSString
sayNum x = return (toJSString ("The number is: " ++ show x ++ "!"))

main :: IO ()
main = pure ()

-- main = do
--   let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
--   let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
--   let srcTxIx = Api.TxIx 0
--   let txIn = ( Api.TxIn srcTxId srcTxIx
--              , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
--              )
--   let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra $ Api.AsConwayEra) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
--   let txOut = Api.TxOut
--                 destAddress
--                 (Api.lovelaceToTxOutValue sbe 10_000_000)
--                 Api.TxOutDatumNone
--                 Script.ReferenceScriptNone
--   let txFee = Api.TxFeeExplicit sbe 2_000_000
--
--
--   let txBodyContent = Api.defaultTxBodyContent sbe
--                     & Api.setTxIns [txIn]
--                     & Api.setTxOuts [txOut]
--                     & Api.setTxFee txFee
--   let (Right txBody) = Api.createTransactionBody sbe txBodyContent
--   let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
--   let witness = Api.WitnessPaymentKey signingKey
--   let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [witness]
--   print oldApiSignedTx
