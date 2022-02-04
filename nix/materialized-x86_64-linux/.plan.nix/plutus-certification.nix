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
      exes = {
        "plutus-certification" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            ];
          buildable = true;
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }