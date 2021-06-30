let

  pkgs = import <nixpkgs> {};

  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    mtl
    hspec
  ]);

in pkgs.stdenv.mkDerivation {
  name = "my-haskell-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}

