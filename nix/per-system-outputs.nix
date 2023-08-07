# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ inputs', pkgs, ... }: 
{
  defaultPackage = inputs'.self.packages.plutus-certification;

  dockerApps = import ../docker-files/docker.nix { inherit inputs' pkgs; };
}