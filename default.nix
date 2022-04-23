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
      # iohkNix overlay needed for cardano-api wich uses a patched libsodium
      haskellNix.overlays ++ iohkNix.overlays.crypto;
  });
  musl64 = pkgs.pkgsCross.musl64;
in
musl64.haskell-nix.project {
  compiler-nix-name = compiler;
  projectFileName = "cabal.project";
  src = musl64.haskell-nix.haskellLib.cleanSourceWith {
    name = "ogmios-src";
    src = ./.;
    subDir = "server";
    filter = path: type:
      builtins.all (x: x) [
        (baseNameOf path != "package.yaml")
      ];
  };
}
