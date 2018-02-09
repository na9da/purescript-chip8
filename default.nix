with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "node";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = with pkgs; [
    nodejs
  ];
  shellHook = "
    export PATH=$PWD/node_modules/.bin:$PATH
  ";
}
