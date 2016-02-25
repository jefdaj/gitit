{ mkDerivation, aeson, base, base64-bytestring, blaze-html
, bytestring, ConfigFile, containers, directory, feed, filepath
, filestore, ghc, ghc-paths, happstack-server, highlighting-kate
, hoauth2, hslogger, HStringTemplate, HTTP, http-client-tls
, http-conduit, json, mtl, network, network-uri, old-locale
, old-time, pandoc, pandoc-types, parsec, pretty, process, random
, recaptcha, safe, SHA, split, stdenv, syb, tagsoup, text, time
, uri, url, utf8-string, uuid, xhtml, xml, xss-sanitize, zlib
, pandoc-citeproc
}:
mkDerivation {
  pname = "gitit";
  version = "0.12.1";
  sha256 = "0wvxynb0bkbbnn9m2m2ivlqjzr2gj9ylp6z5xxpavqakfm70kky2";
  isLibrary = true;
  isExecutable = true;
  jailbreak = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring blaze-html bytestring ConfigFile
    containers directory feed filepath filestore ghc ghc-paths
    happstack-server highlighting-kate hoauth2 hslogger HStringTemplate
    HTTP http-client-tls http-conduit json mtl network network-uri
    old-locale old-time pandoc pandoc-types parsec pretty process
    random recaptcha safe SHA split syb tagsoup text time uri url
    utf8-string uuid xhtml xml xss-sanitize zlib
    pandoc-citeproc
  ];
  executableHaskellDepends = [
    base bytestring directory filepath hslogger HTTP mtl network
    network-uri syb url utf8-string
  ];
  homepage = "http://gitit.net";
  description = "Wiki using happstack, git or darcs, and pandoc";
  license = "GPL";
}
