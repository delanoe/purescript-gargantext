#!/usr/bin/env nix-shell
{ pkgs ? import ./pinned.nix { } }:
let
  packagesJson = ../.psc-package/local/.set/packages.json;
  inputNames = (pkgs.lib.importJSON ../psc-package.json).depends;
  pursPackages = import ./purs-packages.nix { inherit pkgs; };
  getUnquotedSourceGlob = x: ''${x.src}/src/**/*.purs'';
  unquotedSourceGlobs = map getUnquotedSourceGlob (builtins.attrValues pursPackages);
  quote = x: ''"${x}"'';
  sourceGlobs = map quote unquotedSourceGlobs;
in
{
  inherit packagesJson inputNames sourceGlobs unquotedSourceGlobs;
}
