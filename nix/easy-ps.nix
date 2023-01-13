{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "master";
      sha256 = "tESal32bcqqdZO+aKnBzc1GoL2mtnaDtj2y7ociCRGA=";
    }
  ) {
  inherit pkgs;
}
