{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/89c2b2330e733d6cdb5eae7b899326930c2c0648";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectFlakeName = "beam";
          basePackages = pkgs.haskell.packages.ghc98;
          autoWire = ["packages" "checks" "devShells" "apps"];
          devShell.tools = hp: {
            "haskell-language-server" = null;
          };
          packages ={
            pqueue.source = "1.5.0.0";
          };
          settings = {
            pretty-simple = {
              check = false;
            };
          };
        };
      };
    });
}
