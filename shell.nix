# This sets up a development shell with everything needed
# to build the package. Start it with `nix-shell --pure`.
# Because it's "pure" (all dependencies are explicit),
# you need to add your text editor/IDE, git, and any other
# tools to default.nix first or they won't be available.

{ pkgs ? (import <nixpkgs> {}).pkgs }:
(import ./default.nix { inherit pkgs; shell = true; })
