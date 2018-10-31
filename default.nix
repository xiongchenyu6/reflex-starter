# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-starter= ./.;
  };

  shells = {
    ghc = ["reflex-starter"];
    ghcjs = ["reflex-starter"];
  };
  overrides = self: super:
    let
      fast = p: pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck p);
      reflex-dom-storage-version = pkgs.lib.importJSON ./reflex-dom-storage.json;
      reflex-dom-nested-routing-version = pkgs.lib.importJSON ./reflex-dom-nested-routing.json;
      reflex-dom-nested-routing  = pkgs.fetchFromGitHub {
        owner = "3noch";
        repo  = "reflex-dom-nested-routing";
        inherit (reflex-dom-nested-routing-version) rev sha256;
      };

      reflex-dom-storage = pkgs.fetchFromGitHub {
        owner = "qfpl";
        repo  = "reflex-dom-storage";
        inherit (reflex-dom-storage-version) rev sha256;
      };

    in rec {
      reflex-dom-storage = super.callCabal2nix "reflex-dom-storage" reflex-dom-storage {};
      reflex-dom-nested-routing = super.callCabal2nix "reflex-dom-nested-routing" reflex-dom-nested-routing {};
      reflex-starter= fast super.reflex-starter;
    };
})

