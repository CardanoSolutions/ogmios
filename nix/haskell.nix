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
    (haskell-nix.stackProject {
      inherit src;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.stackProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      {
        # Stamp executables with the git revision
        packages = lib.genAttrs ["ogmios"] (name: {
          components.exes.${name}.postInstall = ''
            ${setGitRev}
          '';
        });
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
      {
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Wall" "--ghc-option=-Werror" ]; });
      }
      # Musl libc fully static build
      ({ pkgs, ... }: lib.mkIf stdenv.hostPlatform.isMusl (let
        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
          configureFlags = [
            "--ghc-option=-optl=-lssl"
            "--ghc-option=-optl=-lcrypto"
            "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
          ];
        };
      in
        {
          packages = lib.genAttrs projectPackages (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
      })
    ];
  };
  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''${haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/*'';
  # package with libsodium:
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkgSet
