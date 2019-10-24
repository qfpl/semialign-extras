{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      witherable = super.callHackageDirect {
        pkg = "witherable";
        ver = "0.3.4";
        sha256 = "0g4w59lns7p4mxq93f7b04ycxzh8rww7sd301lx6j9vmza7p1qwy";
      } {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./semialign-merge.nix {})
