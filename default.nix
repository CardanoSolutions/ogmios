{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, gitrev ? null # Git sha1 hash, to be passed when not building from a git work tree.
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride gitrev; }
}:
with pkgs; with commonLib;
let
  haskellPackages =
    recRecurseIntoAttrs (selectProjectPackages ogmiosHaskellPackages);

  packages = {
    inherit haskellPackages;

    inherit (ogmiosHaskellPackages) roots;
    inherit (haskellPackages.ogmios.components.exes) ogmios;
    inherit (haskellPackages.ogmios.project) plan-nix;
    inherit (haskellPackages.ogmios.identifier) version;

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in
  packages
