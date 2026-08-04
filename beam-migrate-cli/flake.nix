{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/89c2b2330e733d6cdb5eae7b899326930c2c0648";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    beam-core.url = "path:../beam-core";
    beam-migrate.url = "path:../beam-migrate";
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
          packages = {
            beam-core.source = inputs.beam-core;
            beam-migrate.source = inputs.beam-migrate;
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
