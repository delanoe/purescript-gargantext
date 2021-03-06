#!/usr/bin/env nix-shell
#!nix-shell ./generate-packages-json.nix --run 'exit'
{ pkgs ? import ./pinned.nix { } }:
let
  easy-dhall = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "288ee825c326f352a5db194a024bd3e1f2f735b2";
      sha256 = "12v4ql1nm1famz8r80k1xkkdgj7285vy2vn16iili0qwvz3i98ah";
    }
  ) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [ easy-dhall.dhall-simple easy-dhall.dhall-json-simple ];

  shellHook = ''
    dhall-to-json --file packages.dhall --output ./.psc-package/local/.set/packages.json
    echo "generated packages.json"
  '';
}
