{
  description = "Certification as a service for Plutus applications";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }: let
    supportedSystems = [ "aarch64-linux"  "x86_64-linux" "x86_64-darwin" "aarch64-darwin"  ];
  in flake-utils.lib.eachSystem supportedSystems (system: let
    overlays = [ haskellNix.overlay ];

    #***************************************************************************************
    # Docker image

    pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
    pkgsLinux = pkgs // { system = "x86_64-linux"; };

    imgAttributes = {
      name = "plutus-certification";
      tag = "8";
    };
    nixConfig = ''
        trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
        substituters = https://hydra.iohk.io https://cache.nixos.org/ https://cache.iog.io
        build-users-group = nixbld
        sandbox = false
        experimental-features = nix-command flakes
        allow-import-from-derivation = true
        filter-syscalls = false
    '';
    entryPoint =
      let addParameter = paramName: varName: '' 
          if [ -n "${"$"}${varName}" ]; then
            args="$args --${paramName} ${"$"}${varName}"
          fi
      '';
      in (pkgs.writeShellScript "entryPoint" ''
        set -eEo pipefail
        args="--local "
        ${addParameter "wallet-id" "WALLET_ID"} \
        ${addParameter "wallet-address" "WALLET_ADDRESS"} \
        ${addParameter "wallet-passphrase" "WALLET_PASSPHRASE"}
        ${addParameter "wallet-url" "WALLET_URL"}
        ${addParameter "wallet-certification-price" "WALLET_CERTIFICATION_PRICE"}
        ${addParameter "gh-access-token" "GH_ACCESS_TOKEN"}
        ${addParameter "signature-timeout" "SIGNATURE_TIMEOUT"}
        ${addParameter "use-whitelist" "USE_WHITELIST"}
        ${addParameter "unsafe-plain-address-auth" "UNSAFE_PLAIN_ADDRESS_AUTH"}
        ${addParameter "port" "PORT"}
        if [ -n "$JWT_SECRET" ];
        then
            args="$args --jwt-secret $JWT_SECRET"
            ${addParameter "jwt-expiration-seconds" "JWT_EXPIRATION"}
        else 
          args="$args --unsafe-plain-address-auth"
        fi

        # create a temporary directory for executing flakes
        mkdir -p /tmp

        # copy the certificate bundle to the right place
        mkdir -p /etc/ssl/certs
        script="cp ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt"
        
        #TODO: change this --unsafe-bypass-subscription-validation
        args="$args --unsafe-bypass-subscription-validation"

        echo $script >&2
        eval "$script"

        script="${flake.packages."plutus-certification:exe:plutus-certification"}/bin/plutus-certification $args"
        echo $script >&2
        eval "$script"
      '').outPath;
  
    nixImage = pkgs.dockerTools.pullImage {
      imageName = "nixos/nix";
      imageDigest = "sha256:31b808456afccc2a419507ea112e152cf27e9bd2527517b0b6ca8639cc423501";
      sha256 = "0bbw3r0civlcm3inj23fq8f25aw63rnaay09qjbrvfjd7pcfbyqn";
      finalImageName = "nixos/nix";
      finalImageTag = "2.15.0";
    };
    genFlake = flake.packages."dapps-certification-helpers:exe:generate-flake";
    buildFlake = flake.packages."dapps-certification-helpers:exe:build-flake";
    
    image = pkgs.dockerTools.buildImage (imgAttributes // {
      fromImage = nixImage;
      diskSize = 5120;
      #contents = [ pkgs.hello ];
      copyToRoot = pkgs.buildEnv {
        name = "image-root";
        paths = [ pkgs.curl pkgs.zsh pkgs.coreutils pkgs.nmon pkgs.cacert genFlake buildFlake ];
        pathsToLink = [ "/bin" ];
      };
       runAsRoot = ''
        rm -rf /etc/nix/nix.conf
        echo "${nixConfig}" > /etc/nix/nix.conf
       '';

      config = {
        Cmd = ["${entryPoint}"];
        User = "root";
      };
    });

    materializedPath = ./. + "/nix/materialized/${system}";

    project = pkgs.haskell-nix.cabalProject' {
      src = ./.;
      compiler-nix-name = "ghc924";
      shell.tools = {
          cabal = {};
          haskell-language-server = {version = "latest";};
          hlint = {};
          stylish-haskell = {};
      };
      materialized = if builtins.pathExists materializedPath then materializedPath else null;
    };

    flake = project.flake {};
    loadDockerImage = {
      type= "app";
      program = (pkgs.writeShellScript "loadDockerImage" ''
          set -eEuo pipefail
          echo "Loading docker image ${image}" >&2
          ${pkgs.docker}/bin/docker load -i ${image}
      '').outPath;
    };
    runDockerImage = 
      let addEnvVar = varName: '' 
          if [ -n "${"$"}${varName}" ]; then
            docker_args="$docker_args -e ${varName}=${"$"}${varName}" 
          fi
      '';
      in {
        type = "app";
        program = (pkgs.writeShellScript "runDockerImage" ''
            set -eEo pipefail
            export PATH="${pkgs.lib.makeBinPath [ pkgs.docker pkgs.coreutils]}"
            echo "Executing ${loadDockerImage.program}..." >&2
            ${loadDockerImage.program}
            docker_args="-t --platform linux/amd64 --name ${imgAttributes.name}"

            ${addEnvVar "WALLET_ID"}
            ${addEnvVar "WALLET_ADDRESS"}
            ${addEnvVar "WALLET_PASSPHRASE"}
            ${addEnvVar "JWT_SECRET"}
            ${addEnvVar "WALLET_URL"}
            ${addEnvVar "WALLET_CERTIFICATION_PRICE"}
            ${addEnvVar "GH_ACCESS_TOKEN"}
            ${addEnvVar "JWT_EXPIRATION"}
            ${addEnvVar "SIGNATURE_TIMEOUT"}
            ${addEnvVar "USE_WHITELIST"}
            ${addEnvVar "UNSAFE_PLAIN_ADDRESS_AUTH"}
            ${addEnvVar "PORT"}

            if [[ -z "$PORT" ]]; then
              export PORT=9671
            fi
            docker_args="$docker_args -p $PORT:$PORT"
            
            script="docker run --rm $docker_args ${imgAttributes.name}:${imgAttributes.tag}"
            echo $script >&2
            eval "$script"
        '').outPath;
      };
      pushDockerImage = {
        type = "app";
        #usage: nix run .\#apps.x86_64-linux.pushDockerImage  -- <docker registry>
        #E.g. nix run .\#apps.x86_64-linux.pushDockerImage  -- ghcr.io/demoiog
        program = (pkgs.writeShellScript "pushDockerImage" ''
            set -eEuo pipefail
            export PATH="${pkgs.lib.makeBinPath [ pkgs.docker pkgs.coreutils]}"
            ${loadDockerImage.program}
            echo "Pushing docker image ${image}" >&2
            imageName="${imgAttributes.name}:${imgAttributes.tag}"

            script="docker image tag $imageName $1/$imageName"
            echo $script >&2
            eval "$script"

            script="docker push $1/$imageName"
            echo $script >&2
            eval "$script"

        '').outPath;
      };
  in flake // {
    packages = flake.packages // {
      inherit (project.plan-nix.passthru) generateMaterialized;
      inherit image;
    };
    defaultPackage = flake.packages."plutus-certification:exe:plutus-certification";
    apps = flake.apps // {
      inherit loadDockerImage;
      inherit runDockerImage;
      inherit pushDockerImage;
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
