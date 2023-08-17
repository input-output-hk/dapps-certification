{ inputs', pkgs, l, ... }: let
    imgAttributes = {
      name = "plutus-certification";
      tag = "12";
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
        ${addParameter "port" "PORT"}
        ${addParameter "db-path" "DB_PATH"}
        if [ -n "$JWT_SECRET" ];
        then
            args="$args --jwt-secret $JWT_SECRET"
            ${addParameter "jwt-expiration-seconds" "JWT_EXPIRATION"}
        else 
          if [ -n "$UNSAFE_PLAIN_ADDRESS_AUTH" ];
          then
            args="$args --unsafe-plain-address-auth"
          else
            args="$args --jwt-generate"
            ${addParameter "jwt-expiration-seconds" "JWT_EXPIRATION"}
          fi
        fi

        # create a temporary directory for executing flakes
        mkdir -p /tmp
        mkdir -p /db

        # copy the certificate bundle to the right place
        mkdir -p /etc/ssl/certs
        script="cp ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt"

        #TODO: change this --unsafe-bypass-subscription-validation
        args="$args --unsafe-bypass-subscription-validation"

        echo $script >&2
        eval "$script"

        script="${inputs'.self.apps.plutus-certification.program} $args"
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
    genFlake = inputs'.self.packages.generate-flake;
    buildFlake = inputs'.self.packages.build-flake;
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
    loadDockerImage = {
      type = "app";
      program = (pkgs.writeShellScript "loadDockerImage" ''
          set -eEuo pipefail
          echo "Loading docker image ${image}" >&2
          ${pkgs.docker}/bin/docker load -i ${image}
      '').outPath;
    };
in
rec {
  inherit loadDockerImage;

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
          export PATH="${l.makeBinPath [ pkgs.docker pkgs.coreutils]}"
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
          ${addEnvVar "DB_PATH"}

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
      # Usage: run .\#dockerApps.pushDockerImage -- <docker registry>
      # Example: .\#dockerApps.pushDockerImage -- ghcr.io/demoiog
      program = (pkgs.writeShellScript "pushDockerImage" ''
          set -eEuo pipefail
          export PATH="${l.makeBinPath [ pkgs.docker pkgs.coreutils]}"
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

    evaluation-test = pkgs.writeText "docker-apps-evaluation-test" ''
      ${runDockerImage.program}
      ${pushDockerImage.program}
    '';
}
