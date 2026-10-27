{
  inputs,
  self,
  ...
}: {
  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    project = pkgs.haskell-nix.cabalProject' ({
      config,
      pkgs,
      ...
    }: {
      src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
        name = "ogmios-src";
        src = self;
        subDir = "server";
        # Filter out package.yaml files so plan-to-nix uses the
        # pre-generated .cabal files instead. The package.yaml files
        # reference ../../.hpack.config.yaml which is outside the
        # server/ subDir and would cause hpack to fail.
        filter = path: type:
          builtins.all (x: x) [
            (baseNameOf path != "package.yaml")
          ];
      };
      name = "ogmios";
      compiler-nix-name = lib.mkDefault "ghc9124";

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
      };

      sha256map = {
        "https://github.com/CardanoSolutions/cardano-ledger.git"."5cca15a1f0629c11e8d4d4daeb73428684f9c34f" = "sha256-gRADCB04orqPB2Lkadus0C1O+/Nm5oJHuddXm8wgx5w=";
        "https://github.com/CardanoSolutions/ouroboros-consensus.git"."5bbbf9c8b4cd3dc3e3a80d13ef54b3a2ee43a585" = "sha256-Pihx/gVf1GVGElAkMeu2AKtIpPfoCj4faQ0DTjajrqc=";
        "https://github.com/CardanoSolutions/ouroboros-network.git"."d3477c4e6b3243f89afb974914ea423a75873fa0" = "sha256-eMfFcV6dFEm10yFyi0wKVE1c0ob4X8UmnhIp5A4W/bE=";
      };

      modules = [
        {
          doHaddock = false;
          packages.ogmios.ghcOptions = ["-Werror"];
          packages.ogmios.components.tests.unit.ghcOptions = ["-Wno-unused-imports"];
          # Substitute Ogmios.Version's `$(gitRevParseHEAD)` TH splice with the
          # flake's `self.rev` so nix-built binaries report a meaningful sha.
          packages.ogmios.components.library.preBuild = ''
            substituteInPlace src/Ogmios/Version.hs \
              --replace-fail '$(gitRevParseHEAD)' '"${self.rev or self.dirtyRev or "0000000000000000000000000000000000000000"}"' \
              --replace-fail '    ( gitRevParseHEAD' '    ('
          '';
          packages.ogmios.components.tests.unit.preCheck = ''
            cp ${self}/server/ogmios.json .
            cp ${self}/server/cardano.json .

            # Test.Path.Util.getProjectRoot uses TH `makeRelativeToProject ""`
            # which bakes the compile-time absolute path
            # /build/ogmios-src-test-unit-root/server into the binary. Recreate
            # that path at runtime with the golden fixtures and the
            # cardano-configurations submodule content.
            mkdir -p /build/ogmios-src-test-unit-root/server/test
            cp -r ${self}/server/test/golden /build/ogmios-src-test-unit-root/server/test/golden
            cp -r ${self}/server/config /build/ogmios-src-test-unit-root/server/config
          '';
        }
        ({pkgs, ...}: {
          # Use the VRF fork of libsodium
          packages = {
            cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [
              [pkgs.libsodium-vrf]
            ];
            cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [
              [pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst]
            ];
          };
        })
        # Musl libc fully static build
        ({
          config,
          lib,
          pkgs,
          ...
        }: {
          options.packages = lib.genAttrs config.package-keys (_:
            lib.mkOption {
              type = lib.types.submodule (
                {
                  config,
                  lib,
                  pkgs,
                  ...
                }:
                  lib.mkIf (pkgs.stdenv.hostPlatform.isMusl && config.package.isLocal)
                  {
                    enableShared = true; # TH code breaks if this is false
                    enableStatic = true;
                  }
              );
            });
          config = lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
            doHaddock = false;
          };
        })
      ];
    });
  in {
    _module.args.project = project;
    _module.args.hsPkgs = project.hsPkgs;
    _module.args.shellFor = args: project.shellFor args;
  };
}
