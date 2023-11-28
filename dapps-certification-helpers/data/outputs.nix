# --sha256: sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= 
{ plutus-apps, dapps-certification, optparse-applicative, plutus-contract-certification, repo, ... }: let
  origProject = repo.iog.x86_64-linux.dapp;

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
        location: https://github.com/pcapriotti/optparse-applicative
        tag: ${optparse-applicative.rev}
        --sha256: sha256-r34aiVBik+acpq9FJ1Nkvw3xf1ldpt1fMHCqvcEgS+k=


      source-repository-package
        type: git
        location: https://github.com/Ali-Hill/plutus-contract-certification
        tag: ${plutus-contract-certification.rev}
        --sha256: ${import (pkgs.stdenv.mkDerivation {
          name = "plutus-contract-certification-sha.nix";
          exportReferencesGraph.plutus-apps = plutus-contract-certification;
          __structuredAttrs = true;
          PATH = pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ];
          builder = builtins.toFile "builder" ''
            . .attrs.sh
            jq '."plutus-apps"[0].narHash' < .attrs.json > "$(jq -r .outputs.out < .attrs.json)"
          '';
        })}

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
in {
  defaultPackage.x86_64-linux = certify;
  packages.x86_64-linux.default = certify;
}
