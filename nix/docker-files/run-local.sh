# execute default.env file and get output string and store it in a variable

#get the current folder where this script is located
currentFolder=$(dirname "$0")
envVarsFile=$($currentFolder/default.env)

script="NIX_CONFIG=\"system = x86_64-linux\" $envVarsFile nix run .#dockerApps.run-docker-image"
echo $script >&2
eval "$script"
