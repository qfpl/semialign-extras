{ nixpkgs ? import ../nix/nixpkgs.nix
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
    overrides = self: super: {
      semialign-merge =
        self.callPackage (import ../semialign-merge/semialign-merge.nix) {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./semialign-diff.nix {})
