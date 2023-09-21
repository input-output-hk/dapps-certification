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

if [ $# -ne 1 ] && [ $# -ne 3 ]; then
  echo "Usage: $0 <brach-name> [--env-file <env-file>]"
  exit 1
fi

# create the base url for the raw files
baseUrl=https://raw.githubusercontent.com/input-output-hk/dapps-certification
folder=nix/docker-files

branch="$1"
# replace slashes with dashes from the branch name
imageTag=${branch//\//-}
echo "Image tag: \"$imageTag\""

imageName=ghcr.io/demoiog/plutus-certification:$imageTag
echo "Image name: \"$imageName\""

#prepare to fetch the env vars but also verify if the branch exists
envVarsFile=default.env
envVarToShell=$baseUrl/$branch/$folder/default.env

echo "Verify the branch \"$branch\" by fetching the env vars from $envVarToShell ..."
response=$(curl -s -o /dev/null -w "%{http_code}" "$envVarToShell")
echo "DONE!"

if [ "$response" -ne 200 ]; then
  echo "Error $response: The branch \"$branch\" could not be found"
  exit 1
fi

# choose the env file between the local one and the remote one
# if the local one is chosen then the remote one is ignored
if [ $# -eq 3 ]; then
    if [ "$2" != "--env-file" ]; then
      echo "Usage: $0 <brach-name> [--env-file <env-file>]"
      exit 1
    fi

    envVarsFile=$3

    # using local env file
    echo "Using local env file: $envVarsFile ..."

    # test if file exists
    if [ ! -f "$envVarsFile" ]; then
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

echo "Checking if the image \"$imageName\" exists..."
script="docker manifest inspect \"$imageName\" > /dev/null 2>&1"
echo $script >&2
eval "$script" || {
  echo "Error: The image \"$imageName\" does not exist!"
  exit 1
}

echo "DONE!"

runDockerFile=run-docker-from-registry.sh
urlToShell=$baseUrl/$branch/$folder/run-docker-from-registry.sh

export $vars
script="curl -sSfL -H 'Cache-Control: no-cache' $urlToShell | bash -s \"$imageName\""
echo $script >&2
eval "$script"
