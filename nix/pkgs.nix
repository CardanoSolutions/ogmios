pkgs: super: with pkgs;
let
  compiler = config.haskellNix.compiler or "ghc8104";
  src = haskell-nix.haskellLib.cleanSourceWith {
    name = "ogmios-src";
    src = ../.;
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
in {
  inherit src;

  ogmiosHaskellPackages = callPackage ./haskell.nix {
    inherit compiler gitrev src;
  };

  inherit (ogmiosHaskellPackages.ogmios.components.exes)
    ogmios;
}
