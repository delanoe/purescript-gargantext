{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "9c5ffd3e54c41dece66ed84f8f23970a4f1f3883";
      sha256 = "8erFzbiRJYqPgJHuQwhgBPltQeaWeAZom/5X3lyUAcc=";
    }
  ) {
  inherit pkgs;
}
