{ reflex-platform ? import ./deps/reflex-platform {}
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  inherit (pkgs) runCommand closurecompiler;
  ghc = reflex-platform.${compiler};
  haskellPackages = ghc.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      cabal = pkgs.haskellPackages.cabalNoTest;
      reflex-dom-contrib =
        pkgs.haskell.lib.doJailbreak (super.callCabal2nix "reflex-dom-contrib" (import ./deps/reflex-dom-contrib {}) {});
      reflex-dom-nested-routing =
        super.callCabal2nix "reflex-dom-nested-routing" (import ./deps/reflex-dom-nested-routing {}) {};
      reflex-dom-storage =
        super.callCabal2nix "reflex-dom-storage" (import ./deps/reflex-dom-storage {}) {};
      reflex-dom-semui =
        super.callCabal2nix "reflex-dom-semui" (import ./deps/reflex-dom-semui {}) {};
      jsaddle-warp =
        pkgs.haskell.lib.dontCheck (super.callCabal2nix "jsaddle-warp" "${import ./deps/jsaddle {}}/jsaddle-warp" {});
    };
  };
  drv = haskellPackages.callPackage ./cabalDeps.nix {};
in
  runCommand "reflex-starter" {} ''
    mkdir -p $out/static
    ${closurecompiler}/bin/closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS  --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=${drv}/bin/reflex-starter.jsexe/all.js.externs ${drv}/bin/reflex-starter.jsexe/all.js  > $out/static/all.js
  ''
