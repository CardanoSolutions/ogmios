{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/0.0.117.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/edb2d2df2ebe42bbdf03a0711115cf6213c9d366.tar.gz")
    { }
, cardanoPkgs ?
    (builtins.fetchTarball
    "https://github.com/input-output-hk/cardano-haskell-packages/archive/316e0a626fed1a928e659c7fc2577c7773770f7f.tar.gz")
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:
let
  pkgs = import nixpkgsSrc (nixpkgsArgs // {
    overlays =
      # iohkNix overlay needed for cardano-api wich uses a patched libsodium
      haskellNix.overlays ++ iohkNix.overlays.crypto;
    });

  mkProject = arch:
    ( arch.haskell-nix.project {
        compiler-nix-name = compiler;
        projectFileName = "cabal.project";
        inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPkgs; };
        src = arch.haskell-nix.haskellLib.cleanSourceWith {
          name = "ogmios-src";
          src = ./.;
          subDir = "server";
          filter = path: type:
            builtins.all (x: x) [
              (baseNameOf path != "package.yaml")
            ];
        };
      }
    );
in {
  platform = {
    arm64 = (mkProject pkgs.pkgsCross.aarch64-multiplatform-musl);
    amd64 = (mkProject pkgs.pkgsCross.musl64).ogmios.components.exes.ogmios;
  };
  tests = {
    unit = (mkProject pkgs.pkgsCross.musl64).ogmios.components.tests.unit;
  };
}
