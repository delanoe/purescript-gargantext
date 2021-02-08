{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "c8c32741bc09e2ac0a94d5140cf51fa5de809e24";
      sha256 = "1g1hlybld298kimd1varvwiflpb0k7sdqlmcqha3kswjvy5z4k6k";
    }
  ) {
  inherit pkgs;
}
