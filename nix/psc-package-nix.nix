{ pkgs ? import ./pinned.nix { } }:

pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "psc-package-nix";
  rev = "e00fdaf2a2628b6b056db12ff4a3069aa04cb536";
  sha256 = "0p2cblmpma7s058r5nfwzpvdsi98qhh5isvc47052wjbmfinh8db";
}
