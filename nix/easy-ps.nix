{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      #owner = "justinwoo";
      owner = "cgenie";
      repo = "easy-purescript-nix";
      rev = "0d34a62a0bc531baa5350dc305fe56317f706be2";
      sha256 = "nkHIyWk5Xh54FqCU7CT3G/tnHF7mLbQt3EfnNCMCTO8=";
    }
  ) {
  inherit pkgs;
}
