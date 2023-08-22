# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ nix, inputs', l, system, ... }: 
{
  defaultPackage = inputs'.self.packages.plutus-certification;

  apps.dockerApps = l.optionalAttrs (system == "x86_64-linux") nix.docker-files.docker; 
}