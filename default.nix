{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/bc471e5235246ca015cd6755adfe1feaa8761498.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/57f8cc93630754eb6f576b587fd0697969a29c75.tar.gz")
    { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2111
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:
let
  pkgs = import nixpkgsSrc (nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
        # needed for cardano-api wich uses a patched libsodium
        ++ iohkNix.overlays.crypto;
  });
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "ogmios-src";
    src = ./.;
    subDir = "server";
    filter = path: type:
      let
         notMatchDir = str: !(builtins.elem str (builtins.split "/" path));
       in
         builtins.all (x: x) [
            (baseNameOf path != "package.yaml")
            (notMatchDir ".stack-work")
            (notMatchDir "cardano-node")
            (notMatchDir "ouroboros-network")
            (notMatchDir "hjsonpointer")
            (notMatchDir "hjsonschema")
            (notMatchDir "wai-routes")
         ];
  };
  projectFileName = "cabal.project";
  compiler-nix-name = compiler;
  modules = [{
    packages = {
      eventful-sql-common = {
        # This is needed so evenful-sql-common will build with a newer version of persistent.
        ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];
        doHaddock = false;
      };

      # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
      plutus-ledger.doHaddock = false;
      plutus-use-cases.doHaddock = false;
    };
  }];
}
