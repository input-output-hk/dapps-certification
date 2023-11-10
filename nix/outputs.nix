{ repoRoot, inputs, lib, system, pkgs }:

let 

  project = lib.iogx.mkHaskellProject {

    cabalProject = pkgs.haskell-nix.cabalProject' {
      name = "dapps-certification";
      src = ../.;
      compiler-nix-name = lib.mkDefault "ghc928";
      shell.withHoogle = false;
      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
      };
    };

    shellArgs = _cabalProject: {
      name = "dapps-cert";
    };
  };

in 

[
  ( 
    project.flake 
  )
  {
    defaultPackage = inputs.self.packages.plutus-certification;
    apps.dockerApps = repoRoot.nix.docker-files.docker; 
  }
  (lib.optionalAttrs (system == "x86_64-linux") 
  {
    hydraJobs.dockerApps.evaluation-test = inputs.self.apps.dockerApps.evaluation-test;
  })
]