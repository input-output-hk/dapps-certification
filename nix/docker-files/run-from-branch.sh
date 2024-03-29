#!/usr/bin/env bash

# DESCRIPTION: This script is used to run the docker image from a specific branch.
# SCOPE: to be used by users not familiar with nix or when the user does not want
# to build the image locally or even clone the repo.
#
# Assumptions: someone else built the image and pushed it to the registry
# through `nix run .\#dockerApps.push-branch-to-docker-registry -- <registry-location>`
#
# Usage: ./run-from-branch.sh <brach-name> [--env-file <env-file>]
# Example: ./run-from-branch.sh master
# Example: ./run-from-branch.sh master --env-file ./my-vars.env
#
# The steps:
# 1. Verify if the branch exists by fetching the env vars from the branch
# 2. Choose the env vars file between the local one and the remote one
# 3. Check if the image exists
# 4. Pull the image
# 5. Run the image with the env vars

set -eEu

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "Docker is not installed. Please install Docker and try again."
    exit 1
fi

# Check if Docker daemon is running
if ! docker info &> /dev/null; then
    echo "Docker daemon is not running. Please start Docker and try again."
    exit 1
fi

echo "Docker is installed, the daemon is running"

if [ $# -ne 1 ] && [ $# -ne 3 ] && [ $# -ne 5 ] && [ $# -ne 7 ]; then
    echo "Usage: $0 <branch-name> [--env-file <env-file>] [--admin-address <address>] [--docker-volume-prefix <volume-prefix-name>]"
    exit 1
fi

# create the base url for the raw files
baseUrl=https://raw.githubusercontent.com/input-output-hk/dapps-certification
folder=nix/docker-files

branch="$1"
shift
# replace slashes with dashes from the branch name
imageTag=${branch//\//-}
echo "Image tag: \"$imageTag\""

imageName=ghcr.io/input-output-hk/plutus-certification:$imageTag
echo "Image name: \"$imageName\""

#prepare to fetch the env vars but also verify if the branch exists
envVarsFile=""
envVarToShell=$baseUrl/$branch/$folder/default.env
adminAddress=""
dockerVolume=""

echo "Verify the branch \"$branch\" by fetching the env vars from $envVarToShell ..."
response=$(curl -s -o /dev/null -w "%{http_code}" "$envVarToShell")
echo "DONE!"

if [ "$response" -ne 200 ]; then
  echo "Error $response: The branch \"$branch\" could not be found"
  exit 1
fi

while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --env-file)
        envVarsFile="$2"
        shift
        shift
        ;;
        --admin-address)
        adminAddress="$2"
        shift
        shift
        ;;
        --docker-volume-prefix)
        dockerVolume="$2"
        shift
        shift
        ;;
        *)
        echo "Unknown argument: $key"
        echo "Usage: $0 <branch-name> [--env-file <env-file>] [--admin-address <address>] [--docker-volume-prefix <volume-prefix-name>]"
        exit 1
        ;;
    esac
done

if [[ -n $envVarsFile ]]; then
    # using local env file
    echo "Using local env file: $envVarsFile ..."

    # test if file exists
    if [[ ! -f "$envVarsFile" ]]; then
        echo "Error: The file \"$envVarsFile\" does not exist!"
        exit 1
    fi

    vars=$($envVarsFile)
    echo "DONE!"
else
    echo "Using remote env file: $envVarToShell"
    # execute that shell script and get output string and store it in a variable
    vars=$(curl -s -H 'Cache-Control: no-cache' "$envVarToShell" | grep -v "^#" | xargs)
    echo "DONE!"
fi

# Use the adminAddress variable if it has been set
if [[ -n $adminAddress ]]; then
    echo "Admin address is set to: $adminAddress"
fi

echo "Setting environment variables ..."

# add the admin address if it has been set (ADMIN_WALLET)
if [[ -n $adminAddress ]]; then
    vars="$vars ADMIN_WALLET=$adminAddress FORCE_ADMIN_ALWAYS=1"
fi

if [[ -n $dockerVolume ]]; then
    vars="$vars DOCKER_VOLUME=$dockerVolume"
fi

echo $vars

echo "Checking if the image \"$imageName\" exists..."
script="docker manifest inspect \"$imageName\" > /dev/null 2>&1"
echo $script >&2
eval "$script" || {
  echo "Error: The image \"$imageName\" does not exist!"
  exit 1
}

echo "DONE!"

export $vars

runDockerFile=run-docker-from-registry.sh
urlToShell=$baseUrl/$branch/$folder/run-docker-from-registry.sh

script="curl -sSfL -H 'Cache-Control: no-cache' $urlToShell | bash -s \"$imageName\""
echo $script >&2
eval "$script"
