{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0ad5775c1e80cdd952527db2da969982e39ff592";
      sha256 = "bwbpXSTD8Hf7tlCXfZuLfo2QivvX1ZDJ1PijXXRTo3Q=";
    }
  ) {
  inherit pkgs;
}
