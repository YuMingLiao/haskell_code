
{ pkgs, mkDerivation, base, cassava, conduit, containers, csv
, dimensional, distance, extra, fake, generic-lens
, google-maps-reflex, lens, matrix, MissingH, numbering
, numeric-prelude, permutation, pretty-simple, QuickCheck, reflex
, reflex-dom, species, split, stdenv, text, type-iso, utility-ht
}:
mkDerivation {
  pname = "BestRoutes";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cassava conduit containers csv dimensional distance extra fake
    generic-lens google-maps-reflex lens matrix MissingH numbering
    numeric-prelude permutation pretty-simple QuickCheck reflex
    reflex-dom species split text type-iso utility-ht
  ];
  license = stdenv.lib.licenses.bsd3;

}
