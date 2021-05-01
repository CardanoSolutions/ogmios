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
      identifier = { name = "git-th"; version = "1.0.0"; };
      license = "MPL-2.0";
      copyright = "2020-2021 KtorZ";
      maintainer = "matthias.benkort@gmail.com";
      author = "KtorZ <matthias.benkort@gmail.com>";
      homepage = "https://github.com/KtorZ/cardano-ogmios#readme";
      url = "";
      synopsis = "Compile-time helpers to access git informations.";
      description = "Please see the README on GitHub at <https://github.com/KtorZ/cardano-ogmios/tree/master/server/modules/git-th>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      tests = {
        "integration" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."git-th" or (errorHandler.buildDepError "git-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../server/modules/git-th; }) // {
    cabal-generator = "hpack";
    }