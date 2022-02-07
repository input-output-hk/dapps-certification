{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.4";
      identifier = { name = "plutus-certification"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "shea.levy@iohk.io";
      author = "Shea Levy";
      homepage = "https://github.com/input-output-hk/plutus-certification#readme";
      url = "";
      synopsis = "";
      description = "Certification as a service for Plutus applications";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          ];
        buildable = true;
        modules = [ "Plutus/Certification/API" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plutus-certification" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."plutus-certification" or (errorHandler.buildDepError "plutus-certification"))
            ];
          buildable = true;
          modules = [ "Paths_plutus_certification" ];
          hsSourceDirs = [ "server" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-certification-client" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."plutus-certification" or (errorHandler.buildDepError "plutus-certification"))
            ];
          buildable = true;
          hsSourceDirs = [ "client" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }