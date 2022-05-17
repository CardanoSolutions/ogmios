{
  description = "cardano-ogmios";

  inputs = {
    haskell-nix.url = "github:L-as/haskell.nix?ref=master";

    nixpkgs.follows = "haskell-nix/nixpkgs-2105";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/41545ba3ac6b3095966316a99883d678b5ab8da8";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/814df2c146f5d56f8c35a681fe75e85b905aed5d";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url =
        "github:input-output-hk/hedgehog-extras/edf6945007177a638fbeb8802397f3a6f4e47c14";
      flake = false;
    };
    hjsonpointer = {
      url =
        "github:KtorZ/hjsonpointer/75ed0d049c33274a6cb4c36c8538d4bf2ef9c30e";
      flake = false;
    };
    hjsonschema = {
      url =
        "github:KtorZ/hjsonschema/fde6e676f79f3f3320a558f20492ad816a2543a7";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/32af91686b86dac7454eee8b8a8d6e97a80638da";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      flake = false;
    };
    wai-routes = {
      url =
        "github:KtorZ/wai-routes/d74b39683792649c01113f40bf57724dcf95c96a";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, ... }@inputs:
    let
      defaultSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system: import nixpkgs {
        overlays = [ haskell-nix.overlay ];
        inherit (haskell-nix) config;
        inherit system;
      };

      # `static` enables cross-compilation with musl64 to produce a statically
      # linked `ogmios` executable
      projectFor = { system, static ? false }:
        let
          pkgs = nixpkgsFor system;
          plutus = import inputs.plutus { inherit system; };
          src = ./.;
        in
        import ./nix/haskell.nix {
          inherit src inputs pkgs plutus system static;
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
      check = perSystem (
        system:
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
      checks = perSystem (
        system:
        {
          inherit (self.flake.${system}.checks) "ogmios:test:unit";
        }
      );
    };
}
