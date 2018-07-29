let
  pkgs = import <nixpkgs> { }; #files
  google-maps-reflex = pkgs.haskellPackages.callPackage ./google-maps-reflex/google-maps-reflex.nix {};

in
  pkgs.haskellPackages.callPackage ./default.nix {inherit google-maps-reflex;} #instructions

