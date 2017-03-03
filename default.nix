# TODO make this a function/package rather than standalone, and add to all
# TODO rename it something besides gitit to keep from messing up the official one
# TODO can makeWrapper be replaced by executableToolDepends?

with import <nixpkgs> {};

let
  gitit = haskellPackages.callPackage ./gitit.nix {};
  myTex = texlive.combine {
    inherit (texlive) scheme-small collection-xetex;
  };

in stdenv.lib.overrideDerivation gitit (oldAttrs: {
  # custom attributes go here so gitit.nix can be overwritten by cabal2nix
  executableToolDepends = [ graphviz myTex ]; # TODO is this used?
  buildInputs = [ makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/gitit \
      --prefix PATH : "${graphviz}/bin" \
      --prefix PATH : "${myTex}/bin"
  '';
})
