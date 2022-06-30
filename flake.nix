{
  description = "ogmios";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix/cf1f0460b65efadac6dc96169ef1e497410fa4f4";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    iohk-nix.url = "github:input-output-hk/iohk-nix/cecab9c71d1064f05f1615eead56ac0b9196bc20";

    # all inputs below here are for use with haskell.nix
    cardano-base = {
      url = "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };
    cardano-crypto = {
      url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url = "github:input-output-hk/cardano-ledger/ce3057e0863304ccb3f79d78c77136219dc786c6";
      flake = false;
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node/9f1d7dc163ee66410d912e48509d6a2300cfa68a";
      flake = false;
    };
    cardano-prelude = {
      url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };
    flat = {
      url = "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url = "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url = "github:input-output-hk/hedgehog-extras/967d79533c21e33387d0227a5f6cc185203fe658";
      flake = false;
    };
    hjsonpointer = {
      url = "github:KtorZ/hjsonpointer/879f0e74d55eef76ceaec8f60ed07657ab84bad7";
      flake = false;
    };
    hjsonschema = {
      url = "github:KtorZ/hjsonschema/35e0b05c3867463363e67f00a5092cd39fa33313";
      flake = false;
    };
    io-sim = {
      url = "github:input-output-hk/io-sim/f4183f274d88d0ad15817c7052df3a6a8b40e6dc";
      flake = false;
    };
    iohk-monitoring-framework = {
      url = "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
      flake = false;
    };
    optparse-applicative = {
      url = "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url = "github:input-output-hk/ouroboros-network/a65c29b6a85e90d430c7f58d362b7eb097fd4949";
      flake = false;
    };
    plutus = {
      url = "github:input-output-hk/plutus/f680ac6979e069fcc013e4389ee607ff5fa6672f";
      flake = false;
    };
    typed-protocols = {
      url = "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };
    wai-routes = {
      url = "github:KtorZ/wai-routes/d74b39683792649c01113f40bf57724dcf95c96a";
      flake = false;
    };
    Win32-network = {
      url = "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system: import nixpkgs {
        overlays = [ haskell-nix.overlay iohk-nix.overlays.crypto ];
        inherit (haskell-nix) config;
        inherit system;
      };

      # `static` enables cross-compilation with musl64 to produce a statically
      # linked `ogmios` executable
      projectFor = { system, static ? false }:
        let
          pkgs = nixpkgsFor system;
          src = ./.;
        in
        import ./nix {
          inherit src inputs pkgs system static;
        };

    in
    {
      flake = perSystem (system: (projectFor { inherit system; }).flake { });

      flake-static = perSystem (system:
        (projectFor { inherit system; static = true; }).flake { }
      );

      defaultPackage = perSystem (system:
        self.flake.${system}.packages."ogmios:exe:ogmios"
      );

      packages = perSystem (system:
        self.flake.${system}.packages // {
          ogmios-static =
            self.flake-static.${system}.packages."ogmios:exe:ogmios";
        }
      );

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's packages and run the `checks`
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages;
          } "touch $out"
      );

      # HACK
      # Only include `ogmios:test:unit` and just build/run that
      # We could configure this via haskell.nix, but this is
      # more convenient
      checks = perSystem (system: {
        inherit (self.flake.${system}.checks) "ogmios:test:unit";
      });
    };
}
