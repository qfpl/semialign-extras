{ mkDerivation, base, doctest, lens, QuickCheck, semialign
, semialign-indexed, stdenv, these, witherable
}:
mkDerivation {
  pname = "semialign-extras";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens semialign semialign-indexed these witherable
  ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "Extra functions for working with Semialigns";
  license = stdenv.lib.licenses.bsd3;
}
