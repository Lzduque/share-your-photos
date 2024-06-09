{
  description = "Share Your Photos";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {
          packageOverrides = pkgs: rec {
            haskellPackages = pkgs.haskellPackages.override {
              overrides = haskellPackagesNew: haskellPackagesOld: rec {
                share-your-photos =
                  haskellPackagesNew.callPackage ./share-your-photos.nix {};
              };
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          inherit config;
        };
      in {
        # nix build .#share-your-photos
        packages.share-your-photos = pkgs.haskellPackages.share-your-photos;

        # nix build
        defaultPackage = self.packages.${system}.share-your-photos;
      }
    );
}
