{ pkgs,flake, ... }: let
    pkgsLinux = pkgs // { system = "x86_64-linux"; };
    imgAttributes = {
      name = "plutus-certification-web";
      tag = "5";
    };
    loadDockerImage = {
      type= "app";
      program = (pkgs.writeShellScript "loadDockerImage" ''
          set -eEuo pipefail
          # build ./Dockerfile.web image and load it into docker
          ${pkgs.docker}/bin/docker build -t ${imgAttributes.name}:${imgAttributes.tag} -f ./Dockerfile.web .
          #${pkgs.docker}/bin/docker save ${imgAttributes.name}:${imgAttributes.tag} | ${pkgs.docker}/bin/docker load
          #and now load the image into docker
          #${pkgs.docker}/bin/docker image ls
      '').outPath;
    };
in {
    loadDockerImage = loadDockerImage;
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
            docker_args="-t --name ${imgAttributes.name} -p 80:3000"

            ${addEnvVar "REACT_APP_BASE_URL"}

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
            echo "Pushing docker image ${imgAttributes:name}:${imgAttributes.tag}" >&2
            imageName="${imgAttributes.name}:${imgAttributes.tag}"

            script="docker image tag $imageName $1/$imageName"
            echo $script >&2
            eval "$script"

            script="docker push $1/$imageName"
            echo $script >&2
            eval "$script"

        '').outPath;
      };
}
