{ mkDerivation, base, doctest, lens, QuickCheck, semialign, stdenv
, these
}:
mkDerivation {
  pname = "semialign-diff";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens semialign these ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "Filterable Semialigns can be diffed and patched";
  license = stdenv.lib.licenses.bsd3;
}
