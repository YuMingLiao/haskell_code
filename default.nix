# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    google-maps-reflex = ./google-maps-reflex;
  };

  shells = {
    ghc = ["common" "backend" "frontend"]; # "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
