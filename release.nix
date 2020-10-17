let
  sources = import ./nix/sources.nix;
in
{ compiler ? "ghc883" # TODO update for ghc884
, pkgs ? import sources.nixpkgs { }
}:
let
  inherit (pkgs.haskell.lib) markUnbroken;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      niv   = import sources.niv {};
      uri   = hpNew.callHackage "uri" "0.1.6.4" {};
      gitit = hpNew.callCabal2nix "gitit" ./. {};
    };
  };
  project = haskellPackages.gitit;
in
{
  project = project;
  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint
    ];
    withHoogle = true;
  };
}
