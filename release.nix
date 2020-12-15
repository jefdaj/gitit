let
  sources = import ./nix/sources.nix;
  nixpkgs = if (import <nixpkgs> {}).stdenv.isDarwin
              then sources.nixpkgs-darwin
              else sources.nixpkgs;
in
{ compiler ? "ghc884" # ideally, this should match the stack lts resolver
, pkgs ? import nixpkgs { }
}:
let
  inherit (pkgs.haskell.lib) markUnbroken;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      niv = import sources.niv {};
      gitit = hpNew.callCabal2nix "gitit" ./. {};

      # keep these up to date with the cabal file,
      # and ideally remove anything that already matches the current nixpkgs
      hoauth           = hpNew.callHackage "hoauth"           "1.11.0"  {};
      hslua            = hpNew.callHackage "hslua"            "1.0.3"   {};
      jira-wiki-markup = hpNew.callHackage "jira-wiki-markup" "1.1.3"   {};
      pandoc           = hpNew.callHackage "pandoc"           "2.9.2.1" {};
      pandoc-types     = hpNew.callHackage "pandoc-types"     "1.20"    {};

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
