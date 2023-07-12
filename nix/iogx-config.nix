# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixiogx-confignix

{ 
  systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
  haskellCompilers = [ "ghc927" ];
  shouldCrossCompile = false;
}
