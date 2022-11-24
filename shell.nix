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

    echo "Installing JS Dependencies"
    yarn

    echo "Compiling"
    #build-purs
    #spago build
    #echo "Bundling"
    #pulp browserify --skip-compile -t dist/bundle.js --src-path output
    #browserify

    # 0.15
    spago bundle-app --main Main --to dist/bundle.js
  '';


  build-css = pkgs.writeShellScriptBin "build-css" ''
    #!/usr/bin/env bash
    set -e

    yarn css

  '';

  serve = pkgs.writeShellScriptBin "serve" ''
    #!/usr/bin/env bash
    set -e

    yarn server

  '';

  build-watch = pkgs.writeShellScriptBin "build-watch" ''
    #!/usr/bin/env bash
    set -e

    echo "Build watch"
    spago build -w --then browserify
  '';

  build-zephyr = pkgs.writeShellScriptBin "build-zephyr" ''
    #!/usr/bin/env bash
    set -e

    spago build --purs-args '--codegen corefn,js'
    zephyr -f Main.main
    browserify-zephyr
  '';

  browserify = pkgs.writeShellScriptBin "browserify" ''
    #!/usr/bin/env bash
    set -e

    pulp browserify --skip-compile -t dist/bundle.js --src-path output
  '';

  browserify-zephyr = pkgs.writeShellScriptBin "browserify-zephyr" ''
    #!/usr/bin/env bash
    set -e

    pulp browserify --skip-compile -t dist/bundle.js -o dce-output
    #purs bundle -o dist/bundle.js -m Main dce-output/**/*.js
  '';

  minify-bundle = pkgs.writeShellScriptBin "minify-bundle" ''
    #!/usr/bin/env bash
    set -e

    minify dist/bundle.js > dist/bundle.min.js
  '';

  repl = pkgs.writeShellScriptBin "repl" ''
    #!/usr/bin/env bash

    spago repl
  '';

  test-ps = pkgs.writeShellScriptBin "test-ps" ''
    #!/usr/bin/env bash
    set -e

    echo "Compiling"
    yarn
    spago build
    #build-purs
    echo "Testing"
    spago test
    # pulp browserify --skip-compile -t dist/bundle.js --src-path output
    # pulp test --src-path output --test-path output
    #NODE_PATH=output node -e "require('Test.Main').main();"
  '';
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_15_4
    easy-ps.psc-package
    easy-ps.dhall-json-simple
    easy-ps.zephyr
    browserify
    browserify-zephyr
    build-css
    build-purs
    build-watch
    build-zephyr
    build
    minify-bundle
    #pkgs.closurecompiler
    pkgs.esbuild
    pkgs.minify
    pkgs.nodejs
    pkgs.python  # needed for msgpack etc
    repl
    serve
    pkgs.pulp
    pkgs.spago
    pkgs.yarn
    test-ps
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
