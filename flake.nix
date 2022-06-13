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
      url = "github:input-output-hk/cardano-base/631cb6cf1fa01ab346233b610a38b3b4cba6e6ab";
      flake = false;
    };
    cardano-crypto = {
      url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url = "github:input-output-hk/cardano-ledger/c415253fff9a5ce9d7575230fc9098bcfee97653";
      flake = false;
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node/82067b797c3f53a9d8adb982622ac58f9d675d24";
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
      url = "github:input-output-hk/io-sim/606de33fa2f467d108fb1efb86daeb3348bf34e3";
      flake = false;
    };
    iohk-monitoring-framework = {
      url = "github:input-output-hk/iohk-monitoring-framework/eb7854d1337637b8672af1227b276aa33a658f47";
      flake = false;
    };
    optparse-applicative = {
      url = "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url = "github:input-output-hk/ouroboros-network/ea202b7b21983140d1944ccfde8750b891b59699";
      flake = false;
    };
    plutus = {
      url = "github:input-output-hk/plutus/fec94223a985e34d3b270460c8f150002f41b85b";
      flake = false;
    };
    typed-protocols = {
      url = "github:input-output-hk/typed-protocols/91c3fba44d68439df207796171cd6f867354b76b";
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
