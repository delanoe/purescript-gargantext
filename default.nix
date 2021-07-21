with (import <nixpkgs> {});
let
  nodejs-with-packages = with nodePackages; [
    bower
    yarn
  ]; in
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv {
    name = name;
    paths = buildInputs;
  };
  buildInputs = [
    nodejs-with-packages
    yarn
    yarn2nix
  ];
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    touch $out
  '';
}
