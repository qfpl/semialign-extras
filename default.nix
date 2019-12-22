{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  baseHaskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = with pkgs.haskell.lib; self: super: {
      semialign = super.semialign_1_1;
      semialign-indexed = unmarkBroken super.semialign-indexed;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./semialign-extras.nix {})
