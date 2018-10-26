{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, cookie, data-default, dependent-map, dependent-sum
, exception-transformers, file-embed, foreign-store, ghcjs-dom
, here, jsaddle, jsaddle-warp, lens, mtl, pureMD5, ref-tf, reflex
, reflex-dom-contrib, reflex-dom-core, reflex-dom-nested-routing
, reflex-dom-storage, stdenv, string-conversions, template-haskell
, text, transformers, uri-bytestring
}:
mkDerivation {
  pname = "reflex-starter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring containers cookie
    data-default dependent-map dependent-sum exception-transformers
    file-embed foreign-store ghcjs-dom here jsaddle jsaddle-warp lens
    mtl pureMD5 ref-tf reflex reflex-dom-contrib reflex-dom-core
    reflex-dom-nested-routing reflex-dom-storage string-conversions
    template-haskell text transformers uri-bytestring
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/reflex-starter#readme";
  license = stdenv.lib.licenses.bsd3;
}
