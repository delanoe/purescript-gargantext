{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "ee51a6d459b8fecfcb10f24ca9728e649c6a9e00";
      sha256 = "dVC+xvdUksFFN0LZYXtKVZPUcVGMAWURGuZ5r1g1k/A=";
    }
  ) {
  inherit pkgs;
}
