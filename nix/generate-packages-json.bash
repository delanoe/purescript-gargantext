#!/usr/bin/env bash
set -e

## You should use the `shell.nix` file in our project, or install dhall-to-json yourself: https://github.com/dhall-lang/dhall-haskell/releases
dhall-to-json --file packages.dhall --output ./.psc-package/local/.set/packages.json
echo "generated packages.json"
