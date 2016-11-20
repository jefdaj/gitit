{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./gitit.nix { graphviz = nixpkgs.pkgs.graphviz; }
