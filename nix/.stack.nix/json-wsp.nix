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
      identifier = { name = "json-wsp"; version = "1.0.0"; };
      license = "MPL-2.0";
      copyright = "2020-2021 KtorZ";
      maintainer = "matthias.benkort@gmail.com";
      author = "KtorZ <matthias.benkort@gmail.com>";
      homepage = "https://github.com/KtorZ/cardano-ogmios#readme";
      url = "";
      synopsis = "An implementation of JSON-WSP in Haskell";
      description = "Please see the README on GitHub at <https://github.com/KtorZ/cardano-ogmios/tree/master/server/modules/json-wsp>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../server/modules/json-wsp; }) // {
    cabal-generator = "hpack";
    }