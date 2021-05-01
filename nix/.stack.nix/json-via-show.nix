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
      identifier = { name = "json-via-show"; version = "1.0.0"; };
      license = "MPL-2.0";
      copyright = "2021 KtorZ";
      maintainer = "matthias.benkort@gmail.com";
      author = "KtorZ <matthias.benkort@gmail.com>";
      homepage = "https://github.com/KtorZ/cardano-ogmios#readme";
      url = "";
      synopsis = "Generic ToJSON instance deriving via Show";
      description = "Please see the README on GitHub at <https://github.com/KtorZ/cardano-ogmios/tree/master/server/modules/json-via-show>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../server/modules/json-via-show;
    }) // { cabal-generator = "hpack"; }