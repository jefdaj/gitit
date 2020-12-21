let
  sources = import ./nix/sources.nix;
  nixpkgs = if (import <nixpkgs> {}).stdenv.isDarwin
              then sources.nixpkgs-darwin
              else sources.nixpkgs;
in
{ compiler ? "ghc883" # matches lts-15.10 resolver in stack.yaml
, pkgs ? import nixpkgs { }
}:
let
  inherit (pkgs.haskell.lib) markUnbroken dontCheck overrideCabal;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {

    # overrides of the whole haskell package set go here
    overrides = hpNew: hpOld: {
      niv = import sources.niv {};
      gitit = hpNew.callCabal2nix "gitit" ./. {};

      # extra-deps from stack.yaml
      base-noprelude   = hpNew.callHackage "base-noprelude"   "4.13.0.0"  {};
      ConfigFile       = hpNew.callHackage "ConfigFile"       "1.1.4"     {};
      doclayout        = hpNew.callHackage "doclayout"        "0.3"       {};
      doctemplates     = hpNew.callHackage "doctemplates"     "0.8.2"     {};
      filestore        = dontCheck (hpNew.callHackage "filestore" "0.6.4" {}); # tests require hg
      happstack-server = hpNew.callHackage "happstack-server" "7.6.0"     {};
      jira-wiki-markup = hpNew.callHackage "jira-wiki-markup" "1.1.4"     {};
      json             = hpNew.callHackage "json"             "0.10"      {};
      MissingH         = hpNew.callHackage "MissingH"         "1.4.3.0"   {};
      pandoc           = dontCheck (hpNew.callHackage "pandoc" "2.9.2.1"  {}); # TODO what's wrong here?
      pandoc-types     = hpNew.callHackage "pandoc-types"     "1.20"      {};
      recaptcha        = hpNew.callHackage "recaptcha"        "0.1.0.4"   {};

      # added to satisfy cabal version bounds
      hslua   = hpNew.callHackage "hslua"   "1.0.3.2" {};
      hoauth2 = hpNew.callHackage "hoauth2" "1.11.0"  {};

    };
  };

  # overrides for the auto-generated gitit package above go here
  project = overrideCabal haskellPackages.gitit (old: {
    executableSystemDepends = with pkgs; [
      graphviz
      texlive.combined.scheme-full # TODO is this overkill? it includes every TeX package
      # TODO system-sendmail?
    ];
  });

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
