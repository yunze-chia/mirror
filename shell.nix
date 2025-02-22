let
  nixpkgs = (fetchTarball channel:nixos-24.11); # ghc 9.6.6
  pkgs = import nixpkgs { };

in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        cabal2nix
        haskell-language-server
      ]);
}
