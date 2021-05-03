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

  # TODO: static binary
  #
  # rewrite-static = _: p: if (pkgs.stdenv.hostPlatform.isDarwin) then
  #   pkgs.runCommandCC p.name {
  #     nativeBuildInputs = [ pkgs.haskellBuildUtils.package pkgs.buildPackages.binutils pkgs.buildPackages.nix ];
  #   } ''
  #     cp -R ${p} $out
  #     chmod -R +w $out
  #     rewrite-libs $out/bin $out/bin/*
  #   '' else if (pkgs.stdenv.hostPlatform.isMusl) then
  #   pkgs.runCommandCC p.name { } ''
  #     cp -R ${p} $out
  #     chmod -R +w $out
  #     $STRIP $out/bin/*
  #   '' else p;
  #
  #  exes =
  #    mapAttrsRecursiveCond (as: !(isDerivation as)) rewriteStatic (collectComponents' "exes" haskellPackages);

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
