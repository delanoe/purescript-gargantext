{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "11d3bd58ce6e32703bf69cec04dc7c38eabe14ba";
      sha256 = "tESal32bcqqdZO+aKnBzc1GoL2mtnaDtj2y7ociCRGA=";
    }
  ) {
  inherit pkgs;
}
