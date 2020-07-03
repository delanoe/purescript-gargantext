{ pkgs ? import ./nix/pinned.nix {} }:
let
  easy-ps = import ./nix/easy-ps.nix { inherit pkgs; };

  purs-packages = import ./purs-packages.nix { inherit pkgs; };

  purs-project = import ./nix/purs-project.nix { inherit pkgs; };

  build-purs = pkgs.writeShellScriptBin "build-purs" ''
    #!/usr/bin/env bash
    purs compile ${toString purs-project.sourceGlobs} "src/**/*.purs" "test/**/*.purs"
  '';

  build = pkgs.writeShellScriptBin "build" ''
    #!/usr/bin/env bash
    set -e

    echo "Compiling"
    build-purs-from-store
    echo "Bundling"
    yarn pulp browserify --skip-compile -t dist/bundle.js --src-path output
  '';

  repl = pkgs.writeShellScriptBin "repl" ''
    #!/usr/bin/env bash

    yarn pulp repl
  '';
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs
    easy-ps.psc-package
    build-purs
    build
    repl
    pkgs.yarn
  ];

  shellHook = ''
    export PURS_IDE_SOURCES='${toString purs-project.unquotedSourceGlobs}'
  '';
}

## how to build the project with nix dependencies:
#
# 1. start a nix shell (e.g. `nix-shell -j 20`, this uses 20 jobs to fetch deps)
# 2. run `yarn` to install npm deps
# 3a. run `build-purs` to build using nix store dependencies, and make sure to update your purescript ide tooling as necesssary
# 3b. or simply use `psc-package` as you might want to anyway
#
# note that the purescript compiler uses filepaths and timestamps, so using the above two commands
# interchangeably will lead to constant rebuilding of the entire project.
#
## how to update purs-packages.nix
#
# 1. run `nix/generate-packages-json.nix` to generate packages.json
# 2. run `nix/generate-purs-packages.nix` to generate purs-packages.nix
