let
  nixpkgs = (fetchTarball channel:nixos-24.11); # ghc 9.6.6
  pkgs = import nixpkgs { };

in
  pkgs.haskellPackages.callPackage ./mirror.nix { }