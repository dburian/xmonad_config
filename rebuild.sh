#!/bin/bash

dir="$(dirname $0)"

cabal update
rm -f .ghc.environment.*

echo Instaling xmonad, xmonad-contrib to \'"$dir"\'
cabal install --package-env="$dir" --lib xmonad xmonad-contrib
cabal install --package-env="$dir" xmonad
