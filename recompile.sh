#!/bin/bash
rm -f dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250327/cardano-wasm-0.1.0.0/x/cardano-wasm/build/cardano-wasm/cardano-wasm.wasm
wasm32-wasi-cabal build all -j16 --ghc-options="-j16"
wasm-dis dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250327/cardano-wasm-0.1.0.0/x/cardano-wasm/build/cardano-wasm/cardano-wasm.wasm | grep export
rm -f cardano-wasm.js
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250327/cardano-wasm-0.1.0.0/x/cardano-wasm/build/cardano-wasm/cardano-wasm.wasm -o cardano-wasm.js
scp dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250327/cardano-wasm-0.1.0.0/x/cardano-wasm/build/cardano-wasm/cardano-wasm.wasm palas@f.palas87.es:~/www/www.palas87.es/public_html/test/
scp cardano-wasm.js palas@f.palas87.es:~/www/www.palas87.es/public_html/test/
