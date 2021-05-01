{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "fast-bech32"; version = "1.0.0"; };
      license = "MPL-2.0";
      copyright = "2021 KtorZ";
      maintainer = "matthias.benkort@gmail.com";
      author = "KtorZ <matthias.benkort@gmail.com>";
      homepage = "https://github.com/KtorZ/cardano-ogmios#readme";
      url = "";
      synopsis = "Fast implementation of the Bech32 encoding format.";
      description = "Please see the README on GitHub at <https://github.com/KtorZ/cardano-ogmios/tree/master/server/modules/fast-bech32>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."fast-bech32" or (errorHandler.buildDepError "fast-bech32"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "encoding" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."fast-bech32" or (errorHandler.buildDepError "fast-bech32"))
            (hsPkgs."random-bytestring" or (errorHandler.buildDepError "random-bytestring"))
            ];
          buildable = true;
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../server/modules/fast-bech32;
    }) // { cabal-generator = "hpack"; }