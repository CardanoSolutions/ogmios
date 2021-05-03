pkgs: super: with pkgs;
let
  compiler = config.haskellNix.compiler or "ghc8104";
  src = haskell-nix.haskellLib.cleanGit {
    name = "ogmios-src";
    src = ../.;
    subDir = "server";
  };
in {
  inherit src;

  ogmiosHaskellPackages = callPackage ./haskell.nix {
    inherit compiler gitrev src;
  };

  inherit (ogmiosHaskellPackages.ogmios.components.exes)
    ogmios;
}
