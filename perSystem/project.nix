{ inputs, self, ... }:

{
  perSystem = { pkgs, lib, ... }:
    let
      project = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
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
        compiler-nix-name = lib.mkDefault "ghc984";

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
            packages.ogmios.ghcOptions = [ "-Werror" ];
          }
          ({ pkgs, ... }: {
            # Use the VRF fork of libsodium
            packages = {
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [
                [ pkgs.libsodium-vrf ]
              ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [
                [ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]
              ];
            };
          })
        ];
      });
    in
    {
      _module.args.hsPkgs = project.hsPkgs;
      _module.args.shellFor = args: project.shellFor args;
    };
}
