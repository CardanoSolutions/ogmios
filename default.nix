{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/4d0cd7c33decb1d9141d5af55c8807f656343d2e.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/a476b842872e7ad924af5cd6a6ca92915a6e41ed.tar.gz")
    { }
, cardanoPkgs ?
    (builtins.fetchTarball
    "https://github.com/input-output-hk/cardano-haskell-packages/archive/62882b0af7cf842652134badcd2d3f8013a85c09.tar.gz")
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:
let
  pkgs = import nixpkgsSrc (nixpkgsArgs // {
    overlays =
      # iohkNix overlay needed for cardano-api wich uses a patched libsodium
      iohkNix.overlays.crypto ++ haskellNix.overlay;
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
