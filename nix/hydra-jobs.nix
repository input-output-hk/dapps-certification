# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#39-nixhydra-jobsnix

{ inputs, pkgs, ... }: 
{
  includedPaths = pkgs.lib.optional pkgs.system.isLinux [
    "dockerApps"
  ];
}