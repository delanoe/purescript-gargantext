{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "678070816270726e2f428da873fe3f2736201f42";
      sha256 = "JEabdJ+3cZEYDVnzgMH/YFsaGtIBiCFcgvVO9XRgiY4=";
    }
  ) {
  inherit pkgs;
}
