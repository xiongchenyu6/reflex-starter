# default.nix
(import ./deps/reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-starter= ./.;
  };

  shells = {
    ghc = ["reflex-starter"];
    ghcjs = ["reflex-starter"];
  };
  overrides = self: super:
    rec {
      reflex-dom-contrib =
        pkgs.haskell.lib.doJailbreak (super.callCabal2nix "reflex-dom-contrib" (import ./deps/reflex-dom-contrib {}) {});
      reflex-dom-nested-routing =
        super.callCabal2nix "reflex-dom-nested-routing" (import ./deps/reflex-dom-nested-routing {}) {};
      reflex-dom-storage =
        super.callPackage (import ./deps/reflex-dom-storage {}) {};
      jsaddle-warp =
        pkgs.haskell.lib.dontCheck (super.callCabal2nix "jsaddle-warp" "${import ./deps/jsaddle {}}/jsaddle-warp" {});
    };
})
