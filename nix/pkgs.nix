pkgs: super: with pkgs;
let
  compiler = config.haskellNix.compiler or "ghc8104";
  src = haskell-nix.haskellLib.cleanGit {
    src = ../.;
    subDir = "server";
    name = "ogmios";
  };
in {
  inherit src;

  ogmiosHaskellPackages = callPackage ./haskell.nix {
    inherit compiler gitrev src;
  };

  inherit (ogmiosHaskellPackages.ogmios.components.exes)
    ogmios;
}
