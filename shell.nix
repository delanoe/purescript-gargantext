{ pkgs ? import ./pinned.nix {} }:
let
  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "14e7d85431e9f9838d7107d18cb79c7fa534f54e";
      sha256 = "0lmkppidmhnayv0919990ifdd61f9d23dzjzr8amz7hjgc74yxs0";
    }
  ) {
    inherit pkgs;
  };

  soba = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "soba";
      rev = "2add8804bce7e7c1ab5eb1c3d8f6783e938a04d3";
      sha256 = "1qagyklcllr2sxdb315prw33af6g37762zgk2ahh3ifxpns6ifxx";
    }
  ) {
    inherit pkgs;
  };

  purs-packages = import ./purs-packages.nix { inherit pkgs; };

  cpPackage = pp:
    let
      target = ".psc-package/local/${pp.name}/${pp.version}";
    in
      ''
        mkdir -p ${target}
        cp --no-preserve=mode,ownership,timestamp -r ${pp.fetched.outPath}/* ${target}
      '';

  install-purs-packages = pkgs.writeShellScriptBin "install-purs-packages" ''
    #!/usr/bin/env bash
    ${builtins.toString (builtins.map cpPackage (builtins.attrValues purs-packages))}
    echo done installing deps.
  '';

  build-purs = pkgs.writeShellScriptBin "build-purs" ''
    #!/usr/bin/env bash
    purs compile "src/**/*.purs" ".psc-package/*/*/*/src/**/*.purs"
  '';

  storePath = x: ''"${x.fetched.outPath}/src/**/*.purs"'';

  build-purs-from-store = pkgs.writeShellScriptBin "build-purs-from-store" ''
    #!/usr/bin/env bash
    purs compile "src/**/*.purs" \
      ${builtins.toString (builtins.map storePath (builtins.attrValues purs-packages))}
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
    soba
    install-purs-packages
    build-purs
    build-purs-from-store
    build
    repl
    pkgs.yarn
  ];
}

## how to build the project with nix dependencies:
#
# 1. start a nix shell (e.g. `nix-shell -j 20`, this uses 20 jobs to fetch deps)
# 2. run `yarn` to install npm deps
# 3. run `install-purs-packages` if you want dependencies locally, available for psc-package and for inspection
# 4. run `build-purs` to build from local sources. otherwise use `build-purs-from-store`.
#
# note that the purescript compiler uses filepaths and timestamps, so using the above two commands
# interchangeably will lead to constant rebuilding of the entire project.
#
## how to update purs-packages.nix
#
# 1. run `soba insdhall` to generate packages.json
# 2. run `soba nix` to generate a nix derivation from packages.json

