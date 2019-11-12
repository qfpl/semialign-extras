{ mkDerivation, base, doctest, lens, QuickCheck, semialign
, semialign-merge, stdenv, these
}:
mkDerivation {
  pname = "semialign-diff";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens semialign semialign-merge these
  ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "Generic diffing and patching for Semialigns";
  license = stdenv.lib.licenses.bsd3;
}
