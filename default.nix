{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./gitit.nix { graphviz = nixpkgs.pkgs.graphviz; }
