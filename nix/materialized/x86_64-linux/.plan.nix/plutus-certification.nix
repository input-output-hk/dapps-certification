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
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cicero-api" or (errorHandler.buildDepError "cicero-api"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-qq" or (errorHandler.buildDepError "aeson-qq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."eventuo11y-batteries" or (errorHandler.buildDepError "eventuo11y-batteries"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."servant-swagger" or (errorHandler.buildDepError "servant-swagger"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."servant-swagger-ui" or (errorHandler.buildDepError "servant-swagger-ui"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."dapps-certification-persistence" or (errorHandler.buildDepError "dapps-certification-persistence"))
          (hsPkgs."dapps-certification-interface" or (errorHandler.buildDepError "dapps-certification-interface"))
          (hsPkgs."dapps-certification-helpers" or (errorHandler.buildDepError "dapps-certification-helpers"))
          ];
        buildable = true;
        modules = [
          "Paths_plutus_certification"
          "Plutus/Certification/API/Routes"
          "Plutus/Certification/API/Swagger"
          "Plutus/Certification/Web3StorageClient"
          "Plutus/Certification/WalletClient/Transaction"
          "Plutus/Certification/Server/TransactionBroadcaster"
          "Plutus/Certification/Server/Internal"
          "Plutus/Certification/Server/Instance"
          "Plutus/Certification/API"
          "Plutus/Certification/Cache"
          "Plutus/Certification/Cicero"
          "Plutus/Certification/Client"
          "Plutus/Certification/Server"
          "Plutus/Certification/Local"
          "Plutus/Certification/GithubClient"
          "Plutus/Certification/WalletClient"
          "Plutus/Certification/Synchronizer"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plutus-certification" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-batteries" or (errorHandler.buildDepError "eventuo11y-batteries"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."plutus-certification" or (errorHandler.buildDepError "plutus-certification"))
            (hsPkgs."dapps-certification-persistence" or (errorHandler.buildDepError "dapps-certification-persistence"))
            (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
            (hsPkgs."servant-swagger-ui" or (errorHandler.buildDepError "servant-swagger-ui"))
            ];
          buildable = true;
          modules = [ "Paths_plutus_certification" ];
          hsSourceDirs = [ "server" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-certification-client" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."plutus-certification" or (errorHandler.buildDepError "plutus-certification"))
            ];
          buildable = true;
          hsSourceDirs = [ "client" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }