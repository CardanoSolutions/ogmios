pkgs: super: with pkgs;
let
  compiler = config.haskellNix.compiler or "ghc8104";
  src = haskell-nix.haskellLib.cleanSourceWith {
    name = "ogmios-src";
    src = ../.;
    subDir = "server";
    filter = path: type: baseNameOf path != "package.yaml";
  };
in {
  inherit src;

  ogmiosHaskellPackages = callPackage ./haskell.nix {
    inherit compiler gitrev src;
  };

  inherit (ogmiosHaskellPackages.ogmios.components.exes)
    ogmios;
}
