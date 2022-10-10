{
  description = "Certification as a service for Plutus applications";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }: let
    supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
  in flake-utils.lib.eachSystem supportedSystems (system: let
    overlays = [ haskellNix.overlay ];

    pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

    materializedPath = ./. + "/nix/materialized/${system}";

    project = pkgs.haskell-nix.cabalProject' {
      src = ./.;
      compiler-nix-name = "ghc924";
      shell.tools = {
          cabal = {};
          haskell-language-server = {};
          hlint = {};
      };
      materialized = if builtins.pathExists materializedPath then materializedPath else null;
    };

    flake = project.flake {};
  in flake // {
    packages = flake.packages // {
      inherit (project.plan-nix.passthru) generateMaterialized;
    };
    defaultPackage = flake.packages."plutus-certification:exe:plutus-certification";
    apps = flake.apps // {
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
}
