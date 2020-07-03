#!/usr/bin/env nix-shell
#!nix-shell ./generate-purs-packages.nix --run 'exit'
{ pkgs ? import ./pinned.nix { } }:
let
  psc-package-nix = import ./psc-package-nix.nix { inherit pkgs; };
  purs-project = import ./purs-project.nix { inherit pkgs; };
  generatePursPackages = import "${psc-package-nix}/nix/generate-purs-packages.nix" {
    inherit pkgs;
    inherit (purs-project) packagesJson inputNames;
  };
in
pkgs.mkShell {
  buildInputs = [ generatePursPackages ];

  shellHook = ''
    generate-purs-packages > ./nix/purs-packages.nix
    echo "generated purs-packages.nix"
  '';
}
