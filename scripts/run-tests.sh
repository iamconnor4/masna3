#!/usr/bin/env bash

set -eao pipefail

[ -f ./environment.test.local.sh ] && source ./environment.test.local.sh || source ./environment.test.sh

if [ -z "$1" ] ;
then
  cabal test all
else
  ghcid --command='cabal repl masna3-test' --test 'Main.main'
fi
