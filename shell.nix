{nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    split
    random
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
  ];
in
pkgs.stdenv.mkDerivation {
  name = "aoc-2019";
  buildInputs = nixPackages;
}
