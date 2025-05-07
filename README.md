# cardano-wasm

Part of an effort at IOG (@Jimbo4350/@palas) to build Cardano Haskell libraries to Wasm.

Enter the Nix shell of the flake (`nix develop`), and then run

```console
wasm32-wasi-cabal update
wasm32-wasi-cabal build
```

```console
wasmtime run "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)"
```
