{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      #owner = "justinwoo";
      owner = "cgenie";
      repo = "easy-purescript-nix";
      rev = "0d34a62a0bc531baa5350dc305fe56317f706be2";
      sha256 = "0brdkkp60l3ggirab0zvhd7q11vk906gaxml7kmbg6gy029j7l5v";
    }
  ) {
  inherit pkgs;
}
