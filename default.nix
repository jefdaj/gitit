# TODO rewrite gitit.sh to use nix rather than custom build
# TODO expand root partition, then add texlive to runDepends
# TODO package canonical-filepath

{ pkgs ? (import <nixpkgs> {}).pkgs
, shell ? false
}:

let
  inherit (pkgs) haskellPackages;

  canonicalFilepath = with haskellPackages;
    cabal.mkDerivation (self: {
      pname = "canonical-filepath";
      version = "1.0.0.3";
      sha256 = "0dg9d4v08gykbjmzafpakgwc51mq5d5m6ilmhp68czpl30sqjhwf";
      buildDepends = [ deepseq filepath ];
      meta = {
        homepage = "http://github.com/nominolo/canonical-filepath";
        description = "Abstract data type for canonical file paths";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      };
    });

  filestore = pkgs.fetchgit {
    url = "https://github.com/jefdaj/filestore.git";
    rev = "1b715eae3df5d925ff65962514572a880b2a56b5";
    sha256 = "0e4fe8d878e772ca0825b66c542608e747dbb1cbbfed22435790eb3b657aab51";
  };

  myDepends = [
    canonicalFilepath
    (import filestore { inherit pkgs; })
  ];

  runDepends = with pkgs; [
    R
    bash
    graphviz
    perl
    procps # includes pkill
    python
  ];

  # put whatever tools you like to use here
  # (only cabal-install is really required)
  shellDepends = if shell
    then with pkgs; [
      git
      haskellPackages.cabalInstall
      less
      vim
    ]
    else [];

# this is just the output of `cabal2nix ./.`
# with some extra dependencies added to buildDepends
in with haskellPackages; cabal.mkDerivation (self: {
  pname = "gitit";
  version = "0.10.5.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = myDepends ++ runDepends ++ shellDepends ++ [
    aeson base64Bytestring blazeHtml ConfigFile feed filepath
    ghcPaths happstackServer highlightingKate hoauth2 hslogger
    HStringTemplate HTTP httpClientTls httpConduit json mtl network
    networkUri pandoc pandocTypes parsec random recaptcha safe SHA
    split syb tagsoup text time uri url utf8String uuid xhtml xml
    xssSanitize zlib
  ];
  meta = {
    homepage = "http://gitit.net";
    description = "Wiki using happstack, git or darcs, and pandoc";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
