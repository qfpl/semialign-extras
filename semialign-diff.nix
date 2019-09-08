{ mkDerivation, base, doctest, QuickCheck, semialign
, semialign-indexed, stdenv, these, witherable
}:
mkDerivation {
  pname = "semialign-diff";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base semialign semialign-indexed these witherable
  ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "Filterable Semialigns can be diffed and patched";
  license = stdenv.lib.licenses.bsd3;
}
