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
        name = "dapps-certification-signature-verification";
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
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."webauthn" or (errorHandler.buildDepError "webauthn"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."blake2" or (errorHandler.buildDepError "blake2"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          ];
        buildable = true;
        modules = [
          "IOHK/Certification/SignatureVerification/CBOR"
          "IOHK/Certification/SignatureVerification"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../dapps-certification-signature-verification;
    }