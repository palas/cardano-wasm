module Main (add42, sayNum, main) where

import GHC.Wasm.Prim

foreign export javascript "add42"
    add42 :: Int -> IO Int
add42 x = return (x + 42)

foreign export javascript "sayNum"
    sayNum :: Int -> IO JSString
sayNum x = return (toJSString ("The number is: " ++ show x ++ "!"))

main :: IO ()
main = pure ()

