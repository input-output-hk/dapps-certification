# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Certification as a service for Plutus applications";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
  };

<<<<<<< HEAD
  outputs = { self, nixpkgs, flake-utils, haskellNix }: let
    supportedSystems = [ "aarch64-linux"  "x86_64-linux" "x86_64-darwin" "aarch64-darwin"  ];
  in flake-utils.lib.eachSystem supportedSystems (system: let
    overlays = [ haskellNix.overlay ];
=======
>>>>>>> 6220e00 (IOGX Integation)

  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;


  nixConfig = {

<<<<<<< HEAD
    flake = project.flake {};

    dockerApps = import ./docker-files/docker.nix { pkgs = pkgs; flake = flake; };

  in flake // {
    packages = flake.packages // {
      inherit (project.plan-nix.passthru) generateMaterialized;
    };
    defaultPackage = flake.packages."plutus-certification:exe:plutus-certification";
    apps = flake.apps // dockerApps // {
      updateAllMaterialized = {
        type = "app";
        program = (pkgs.writeShellScript "updateAllMaterialized" ''
          set -eEuo pipefail
          export PATH="${pkgs.lib.makeBinPath [ pkgs.nix pkgs.jq pkgs.coreutils pkgs.git ]}"
          export NIX_CONFIG="
            allow-import-from-derivation = true
            experimental-features = flakes nix-command
          "
          ${builtins.concatStringsSep "\n" (map (system: ''
            script="$(nix build .#packages.${system}.generateMaterialized --json | jq -r '.[0].outputs.out')"
            echo "Running $script on ./nix/materialized/${system}" >&2
            "$script" "./nix/materialized/${system}"
          '') supportedSystems)}
        '').outPath;
      };
    };
  });
=======
    extra-substituters = [
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    allow-import-from-derivation = true;
  };
>>>>>>> 6220e00 (IOGX Integation)
}
