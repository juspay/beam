# Autogenerated from euler.yaml. Do not edit.

{
  description = "beam";
  inputs = {
    # Laziness of nix allows us to be lazy here and avoid resolving deps
    # The downside is that most of this .follows are redundant
    
  };

  outputs = flakeInputs@{ self, euler-build, ... }:
    euler-build.mkEulerFlake {
      overlayPath = ./nix/overlay.nix;
      extraOverlayPaths = [
        
      ];
      mkConfig = { nixpkgs }: {
        flakeName = "beam";
        defaultPackageName = "beam-core";
        exportPackages = [
          "beam-core"
          "beam-migrate"
          "beam-migrate-cli"
          "beam-postgres"
          "beam-sqlite"
        ];
        shellTools = with nixpkgs; [
          
        ];
        # shellAttrs = {
        # };
      };
      inputs = flakeInputs;
    };
}
