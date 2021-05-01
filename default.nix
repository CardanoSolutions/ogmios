{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, customConfig ? {}
, sourcesOverride ? {}
, gitrev ? null # Git sha1 hash, to be passed when not building from a git work tree.
, pkgs ? import ./nix { inherit system crossSystem config customConfig sourcesOverride gitrev; }
}:
with pkgs; with commonLib;
let
  haskellPackages =
    recRecurseIntoAttrs (selectProjectPackages ogmiosHaskellPackages);

  rewrite-static = _: p: if (pkgs.stdenv.hostPlatform.isDarwin) then
    pkgs.runCommandCC p.name {
      nativeBuildInputs = [ pkgs.haskellBuildUtils.package pkgs.buildPackages.binutils pkgs.buildPackages.nix ];
    } ''
      cp -R ${p} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/*
    '' else if (pkgs.stdenv.hostPlatform.isMusl) then
    pkgs.runCommandCC p.name { } ''
      cp -R ${p} $out
      chmod -R +w $out
      $STRIP $out/bin/*
    '' else p;

  packages = {
    inherit haskellPackages scripts environments;

    inherit (ogmiosHaskellPackages) roots;
    inherit (haskellPackages.ogmios.project) plan-nix;
    inherit (haskellPackages.ogmios.identifier) version;

    exes =
      mapAttrsRecursiveCond (as: !(isDerivation as)) rewriteStatic (collectComponents' "exes" haskellPackages);

    checks = {};

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in
  packages
