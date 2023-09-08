{ plutus-apps, dapps-certification, repo, ... }: let

  mkCertity = system: let 

    origProject = repo.iog.${system}.dapp;

    inherit (origProject) pkgs;

    modifiedCabalProject = pkgs.runCommand "cabal.project" {} ''
      mkdir -p $out
      echo ${pkgs.lib.escapeShellArg origProject.args.cabalProject} | sed 's|^  *plutus-contract-certification$||g' > $out/cabal.project
    '';

    project = origProject.appendModule ({ lib, ... }: {
      cabalProject = lib.mkForce (builtins.readFile "${modifiedCabalProject}/cabal.project");
      cabalProjectLocal = lib.mkForce ((x : if isNull x then "" else x)(origProject.args.cabalProjectLocal) + ''
        source-repository-package
          type: git
          location: https://github.com/input-output-hk/plutus-apps
          tag: ${plutus-apps.rev}
          --sha256: ${import (pkgs.stdenv.mkDerivation {
            name = "plutus-apps-sha.nix";
            exportReferencesGraph.plutus-apps = plutus-apps;
            __structuredAttrs = true;
            PATH = pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ];
            builder = builtins.toFile "builder" ''
              . .attrs.sh
              jq '."plutus-apps"[0].narHash' < .attrs.json > "$(jq -r .outputs.out < .attrs.json)"
            '';
          })}
          subdir:
            plutus-contract-certification
        source-repository-package
          type: git
          location: https://github.com/input-output-hk/dapps-certification
          tag: ${dapps-certification.rev}
          --sha256: ${import (pkgs.stdenv.mkDerivation {
            name = "dapps-certification-sha.nix";
            exportReferencesGraph.plutus-apps = dapps-certification;
            __structuredAttrs = true;
            PATH = pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ];
            builder = builtins.toFile "builder" ''
              . .attrs.sh
              jq '."plutus-apps"[0].narHash' < .attrs.json > "$(jq -r .outputs.out < .attrs.json)"
            '';
          })}
          subdir:
            dapps-certification-interface
        '');
      materialized = lib.mkForce null;
    });

    ghc = project.ghcWithPackages (p: [ p.plutus-contract-certification p.certification p.dapps-certification-interface p.aeson p.async p.unix ]);

    certify = pkgs.runCommand "certify" {} ''
      mkdir -p $out/bin
      ${ghc}/bin/ghc ${./Certify.hs} -Wall -o $out/bin/certify -L${pkgs.numactl}/lib
    '';
  in 
    certify;

in rec {
  defaultPackage.x86_64-linux = mkCertify "x86_64-linux";
  defaultPackage.x86_64-darwin = mkCertify "x86_64-darwin";

  packages.x86_64-linux.default = defaultPackage.x86_64-linux;
  packages.x86_64-darwin.default = defaultPackage.x86_64-darwin;
}
