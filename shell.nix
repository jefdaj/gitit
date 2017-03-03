# TODO is nix required to get the runtime dependencies?
#      if so, probably want both stack *inside* nix-shell!
#      buildStackProject seems to support passthrough of all args,
#      so that should be totally doable

{ghc}:
with (import <nixpkgs> {});

let
  myTexLive = texlive.combine {
    inherit (texlive) scheme-small collection-xetex;
  };

in haskell.lib.buildStackProject {
  inherit ghc;
  name = "gititEnv";
  buildInputs = [ zlib myTexLive ]; # TODO propogate i guess?

  # extra depdendencies for plugins
  # buildDepends = [ graphviz myTexLive makeWrapper ];
  # postInstall = ''
  #   wrapProgram $out/bin/gitit \
  #     --prefix PATH : "${graphviz}/bin:${myTexLive}/bin"
  # '';

}
