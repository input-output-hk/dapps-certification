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
      specVersion = "2.4";
      identifier = {
        name = "dapps-certification-persistence";
        version = "0.1.0.0";
        };
      license = "NONE";
      copyright = "";
      maintainer = "bogdan.manole@iohk.io";
      author = "Bogdan Manole";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
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
          (hsPkgs."selda" or (errorHandler.buildDepError "selda"))
          (hsPkgs."selda-sqlite" or (errorHandler.buildDepError "selda-sqlite"))
          (hsPkgs."selda-postgresql" or (errorHandler.buildDepError "selda-postgresql"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        modules = [
          "IOHK/Certification/Persistence/Structure"
          "IOHK/Certification/Persistence/API"
          "IOHK/Certification/Persistence"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../dapps-certification-persistence; }