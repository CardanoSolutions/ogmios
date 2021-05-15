{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
    customConfig = {
      inherit withHoogle;
    };
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or lorri.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = ogmiosProject.shellFor {
    name = "ogmios-shell";

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabalWrapped
      nixWrapped
      pkgconfig
      pkgs.git
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit (customConfig) withHoogle;
  };

in
 shell
