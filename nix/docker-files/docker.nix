{ inputs', pkgs, l, ... }: let
    imgAttributes = {
      name = "plutus-certification";
      tag = "21";
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
        ${addParameter "min-amount-for-address-reservation" "MIN_AMOUNT_FOR_ADDRESS_RESERVATION"}
        ${addParameter "github-client-id" "GITHUB_CLIENT_ID"}
        ${addParameter "github-client-secret" "GITHUB_CLIENT_SECRET"}
        ${addParameter "admin-wallet" "ADMIN_WALLET"}

        # if FORCE_ADMIN_ALWAYS is set to 1 then add the parameter force-admin-always
        if [ -n "${"$"}FORCE_ADMIN_ALWAYS" ] && [ "${"$"}FORCE_ADMIN_ALWAYS" -eq 1 ]; then
          args="$args --force-admin-always"
        fi
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
    headerScript = {
      type = "app";
      program = (pkgs.writeShellScript "docker-image-header-script" ''
          set -eEuo pipefail
          imageTag="${imgAttributes.tag}"

          # ensure there is at least one argument provided and not more then two
          if [ $# -eq 0 ]; then
              echo "Usage: $0 <docker registry> [<docker image tag>]"
              exit 1
          elif [ $# -gt 2 ]; then
              echo "Too many arguments. Usage: $0 <docker registry> [<docker image tag>]"
              exit 1
          elif [ $# -eq 2 ]; then
              echo "Using provided image tag $2" >&2
              imageTag="$2"
          fi
      '').outPath;
    };
    runDockerArgs =
      let addEnvVar = varName: ''
          if [ -n "${"$"}${varName}" ]; then
            docker_args="$docker_args -e ${varName}=${"$"}${varName}"
          fi
      '';
      in ''
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
          ${addEnvVar "MIN_AMOUNT_FOR_ADDRESS_RESERVATION"}
          ${addEnvVar "GITHUB_CLIENT_ID"}
          ${addEnvVar "GITHUB_CLIENT_SECRET"}
          ${addEnvVar "ADMIN_WALLET"}
          ${addEnvVar "FORCE_ADMIN_ALWAYS"}
          if [[ -z "$PORT" ]]; then
            export PORT=9671
          fi
          docker_args="$docker_args -p $PORT:$PORT"
    '';
    runDockerFromRegistryScript = (pkgs.writeText "run-docker-from-registry-script" ''
          #NOTE: This file is automatically generated running directly
          # `nix run .\#dockerApps.write-run-docker-from-registry`
          # or consequently running `nix run .\#dockerApps.push-branch-to-docker-registry`
          # if the script content has changed

          # there should be one and only one argument
          if [ "$#" -ne 1 ]; then
            echo "Usage: $0 <docker image>"
            exit 1
          fi

          dockerImage="$1"

          # pull down the image

          script="docker pull $dockerImage"
          echo $script >&2
          eval "$script"

          # Use basename to remove the registry from the image name
          containerName=$(basename "$dockerImage")
          #remove the ':' character and replace it with '_'
          containerName=${"$"}{containerName//:/_}

          docker_args="-t --platform linux/amd64 --name $containerName --restart on-failure"

          ${runDockerArgs}

          script="docker run $docker_args $dockerImage"
          echo $script >&2
          eval "$script"
        '');

in
rec {
  load-docker-image = loadDockerImage;

    run-docker-image = {
      type = "app";
      program = (pkgs.writeShellScript "run-docker-image" ''
          set -eEo pipefail
          export PATH="${l.makeBinPath [ pkgs.docker pkgs.coreutils]}"
          echo "Executing ${loadDockerImage.program}..." >&2
          ${loadDockerImage.program}
          docker_args="-t --platform linux/amd64 --name ${imgAttributes.name}"

          ${runDockerArgs}

          script="docker run --rm $docker_args ${imgAttributes.name}:${imgAttributes.tag}"
          echo $script >&2
          eval "$script"
      '').outPath;
    };

    # pushes the docker image to the given registry
    push-docker-image = {
      type = "app";
      # Usage: nix run .\#dockerApps.push-docker-image -- <docker registry> [<docker image tag>]
      # Example: nix run .\#dockerApps.push-docker-image -- ghcr.io/demoiog "1.0.0"
      program = (pkgs.writeShellScript "push-docker-image" ''
          source ${headerScript.program}

          export PATH="${l.makeBinPath [ pkgs.docker pkgs.coreutils]}"
          ${loadDockerImage.program}
          echo "Pushing docker image ${image}" >&2
          originalImageName="${imgAttributes.name}:${imgAttributes.tag}"
          imageName="${imgAttributes.name}:$imageTag"

          script="docker image tag $originalImageName $1/$imageName"
          echo $script >&2
          eval "$script"

          script="docker push $1/$imageName"
          echo $script >&2
          eval "$script"

      '').outPath;
    };

    # get the script from runDockerFromRegistryScript and write to a file
    write-run-docker-from-registry = {
      type = "app";
      program = (pkgs.writeShellScript "write-run-docker-from-registry" ''
          #Description: Writes the script from runDockerFromRegistryScript to a file
          # The file will be pulled down from the chosen branch running the script
          # from `nix/docker-files/run-from-branch.sh`

          set -eEo pipefail
          export PATH="${l.makeBinPath [ pkgs.coreutils]}"

          scriptPath="${runDockerFromRegistryScript}"
          echo $scriptPath >&2

          # read the script from $scriptPath and write it to a /nix/docker-files
          fileDestination=./nix/docker-files/run-docker-from-registry.sh
          cat $scriptPath > $fileDestination
          echo "Wrote script to $fileDestination" >&2
          chmod +x $fileDestination
      '').outPath;
    };

    push-branch-to-docker-registry = {
      type = "app";
      # Usage: nix run .\#dockerApps.push-branch-to-docker-registry -- <docker registry>
      # Example: nix run .\#dockerApps.push-branch-to-docker-registry -- ghcr.io/demoiog
      program = (pkgs.writeShellScript "push-branch-to-docker-registry" ''
          # extract the current branch name and print it
          branch=$(git rev-parse --abbrev-ref HEAD)
          echo "Current branch: $branch"

          # if there is no branch name, exit
          if [ -z "$branch" ]; then
            echo "No branch name found. Exiting..."
            exit 1
          fi
          # before writing the 'run-docker-from-registry.sh' verify if the
          # file has been modified (verification with git) and abort pushing the docker image if so
          ${write-run-docker-from-registry.program}
          fileDestination=./nix/docker-files/run-docker-from-registry.sh
          if [[ $(git status --porcelain $fileDestination) ]]; then
            echo "The file $fileDestination has been modified. Please commit the changes before pushing the docker image." >&2
            exit 1
          fi

          # verify if "default.env" has been modified and abort pushing the docker image if so
          fileDestination=./nix/docker-files/default.env
          if [[ $(git status --porcelain $fileDestination) ]]; then
            echo "The file $fileDestination has been modified. Please commit the changes before pushing the docker image." >&2
            exit 1
          fi

          # if the branch is not up to date with the remote, abort as well
          # NOTE: we ignore the unstaged changes
          echo "Checking if the branch is up to date with the remote..." >&2
          git fetch
          if [[ $(git rev-parse HEAD) != $(git rev-parse @{u}) ]]; then
            echo "The branch is not up to date with the remote. Please pull the changes before pushing the docker image." >&2
            exit 1
          fi
          echo "DONE!" >&2

          # run the tests
          echo "Running the tests..." >&2

          script="${inputs'.self.apps.plutus-certification-test-plutus-certification-test.program} $args"
          echo $script >&2
          eval "$script"

          #if the tests failed, abort
          if [ $? -ne 0 ]; then
            echo "Tests failed. Aborting." >&2
            exit 1
          fi

          # replace slashes with dashes from the branch name
          branch=${"$"}{branch//\//-}
          echo "Current branch: $branch"

          # if there are not two arguments, print usage and exit
          # else print both arguments and exit
          if [ "$#" -ne 1 ]; then
            echo "Usage: $0 <docker registry>"
            exit 1
          fi

          echo "Docker registry: $1"
          echo "Docker image name: $branch"

          # now push the docker image into the docker registry with ${push-docker-image.program}
          # and the provided arguments
          script="${push-docker-image.program} $1 $branch"
          echo $script >&2
          eval "$script"
      '').outPath;
    };

    evaluation-test = pkgs.writeText "docker-apps-evaluation-test" ''
      ${run-docker-image.program}
      ${push-docker-image.program}
      ${push-branch-to-docker-registry.program}
      ${write-run-docker-from-registry.program}
    '';
}
