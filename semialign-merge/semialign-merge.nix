{ mkDerivation, base, containers, doctest, lens, QuickCheck
, semialign, semialign-indexed, stdenv, these, witherable
}:
mkDerivation {
  pname = "semialign-merge";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens semialign semialign-indexed these witherable
  ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "High-performance merging for Semialigns";
  license = stdenv.lib.licenses.bsd3;
}
