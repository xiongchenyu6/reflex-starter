# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-starter= ./.;
  };

  shells = {
    ghc = ["reflex-starter"];
    ghcjs = ["reflex-starter"];
  };
})
