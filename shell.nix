let
  reflex-platform = import ./deps/reflex-platform {};
  nixpkgs = reflex-platform.nixpkgs;
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit reflex-platform; compiler = "ghc"; };
  drvWithTools = 
    pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.addBuildDepends drv [
      pkgs.cabal-install
      pkgs.chromium
      reflex-platform.ghc.ghcid
    ]) {
    };
in
  drvWithTools.env
