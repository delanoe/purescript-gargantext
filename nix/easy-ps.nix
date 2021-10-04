{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "d0f592b71b2be222f8dcfb4f4cefb52608bbc1ae";
      sha256 = "0bq26y836bd1y8657f182wnsl4cdr1xxbykxdgz7xm9shpii48r5";
    }
  ) {
  inherit pkgs;
}
