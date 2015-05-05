# The middle part of this expression can be embedded in nixpkgs;
# the top and bottom adapt it to use the current system instead.

with (import <nixpkgs> {}).pkgs;
with haskellPackages;
let

  # these dependencies are added automatically if you use my nixpkgs repo
  canonicalFilepath =
    # this is the output of `cabal2nix cabal://canonical-filepath`
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
  filestore = fetchgit {
    url = "https://github.com/jefdaj/filestore.git";
    rev = "1b715eae3df5d925ff65962514572a880b2a56b5";
    sha256 = "0e4fe8d878e772ca0825b66c542608e747dbb1cbbfed22435790eb3b657aab51";
  };
  gitit =

    # this part is the complete default.nix when added to mypkgs
    { cabal, aeson, base64Bytestring, blazeHtml, ConfigFile, feed
    , filepath, filestore, ghcPaths, happstackServer, highlightingKate
    , hoauth2, hslogger, HStringTemplate, HTTP, httpClientTls
    , httpConduit, json, mtl, network, networkUri, pandoc, pandocTypes
    , parsec, random, recaptcha, safe, SHA, split, syb, tagsoup, text
    , time, uri, url, utf8String, uuid, xhtml, xml, xssSanitize, zlib
    , graphviz, lmodern, canonicalFilepath
    }:
    cabal.mkDerivation (self: {
      pname = "gitit";
      version = "0.10.6.1";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      jailbreak = true;
      buildDepends = [
        aeson base64Bytestring blazeHtml ConfigFile feed filepath filestore
        ghcPaths happstackServer highlightingKate hoauth2 hslogger
        HStringTemplate HTTP httpClientTls httpConduit json mtl network
        networkUri pandoc pandocTypes parsec random recaptcha safe SHA
        split syb tagsoup text time uri url utf8String uuid xhtml xml
        xssSanitize zlib
        canonicalFilepath graphviz
      ];
      meta = {
        homepage = "http://gitit.net";
        description = "Wiki using happstack, git or darcs, and pandoc";
        license = "GPL";
        platforms = self.ghc.meta.platforms;
      };
    })

; in callPackage gitit {
  inherit canonicalFilepath filestore;
}
