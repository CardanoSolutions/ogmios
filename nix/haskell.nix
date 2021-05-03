{ lib
, stdenv
, haskell-nix
, buildPackages
, src
, config ? {}
, compiler ? config.haskellNix.compiler or "ghc8104"
, profiling ? config.haskellNix.profiling or false
, gitrev ? null
}:
let
  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      {
        packages = lib.genAttrs projectPackages (name: {});
        doHaddock = false;
        doCheck = false;
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        enableLibraryProfiling = profiling;
      }
    ];
  };
in
  pkgSet
