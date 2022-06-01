{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "cbcb53725c430de4e69f652d69c1677e17c6bcec";
      sha256 = "K81AOJe2YmvJ8sSDZAL2Jw85Q9FvRQQCokxBpuNGrpQ=";
    }
  ) {
  inherit pkgs;
}
