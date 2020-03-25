{ pkgs ? import <unstable> {} }: with pkgs;
haskell.lib.buildStackProject {
  name = "kraken-env";
  ghc = haskell.compiler.ghc883;
  buildInputs = [ 
    zlib
  ];
}
