# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {

  overrides = self: super: {
   heist = pkgs.haskell.lib.doJailbreak super.heist;
   map-syntax = pkgs.haskell.lib.doJailbreak super.map-syntax;
  };

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    google-maps-reflex = ./google-maps-reflex;
  };

  shells = {
    ghc = ["common" "backend" "frontend" "heist"]; # "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
