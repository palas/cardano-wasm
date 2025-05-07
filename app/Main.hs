module Main where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Tx as L

main :: IO ()
main = print tx
  where
    tx :: L.Tx L.ConwayEra
    tx = L.mkBasicTx L.mkBasicTxBody
