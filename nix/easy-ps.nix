{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "7ebddd8613cf6736dbecef9fce4c32f2a104ef82";
      sha256 = "1g1hlybld298kimd1varvwiflpb0k7sdqlmcqha3kswjvy5z4k6k";
    }
  ) {
  inherit pkgs;
}
