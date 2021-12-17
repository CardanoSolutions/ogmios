{
  description = "cardano-ogmios";

  inputs = {
    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };

    # The Plutus "flake" isn't really a flake - it doesn't define any
    # outputs and is only used for input pinning according to to
    # the comments
    plutusSrc = {
      type = "github";
      owner = "input-output-hk";
      repo = "plutus";
      rev = "1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      flake = false;
    };

    haskell-nix = {
      type = "github";
      owner = "input-output-hk";
      repo = "haskell.nix";
      # FIXME rev taken from Plutus doesn't work?
      rev = "64cd5f70ce0d619390039a1a3d57c442552b0924";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    flake-compat = {
      type = "github";
      owner = "edolstra";
      repo = "flake-compat";
      rev = "12c64ca55c1014cdc1b16ed5a804aa8576601ff2";
      flake = false;
    };
  };

  outputs = { self, flake-utils, plutusSrc, nixpkgs, haskell-nix, ... }:
    let
      inherit (flake-utils.lib) defaultSystems;

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system: import nixpkgs {
        overlays = [ haskell-nix.overlay ];
        inherit (haskell-nix) config;
        inherit system;
      };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import plutusSrc { inherit system; };
          fakeSrc = pkgs.runCommand "real-src" {} ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
            cat $out/nix/haskell-nix-cabal.project >> $out/cabal.project
          '';
          src = fakeSrc.outPath;
        in
          import ./nix/haskell.nix {
            inherit src pkgs plutus system;
          };

    in
      {
        flake = perSystem (system: (projectFor system).flake {});

        defaultPackage = perSystem (
          system:
            self.flake.${system}.packages."ogmios:exe:ogmios"
        );

        defaultApp = perSystem (
          system:
            self.flake.${system}.packages."ogmios:exe:ogmios"
        );

        packages = perSystem (system: self.flake.${system}.packages);

        apps = perSystem (system: self.flake.${system}.apps);

        devShell = perSystem (system: self.flake.${system}.devShell);

        # This will build all of the project's executables and the tests
        check = perSystem (
          system:
            (nixpkgsFor system).runCommand "combined-executables" {
              nativeBuildInputs = builtins.attrValues self.checks.${system};
            } "touch $out"
        );

        # NOTE `nix flake check` will not work at the moment due to use of
        # IFD in haskell.nix
        #
        # Includes all of the packages in the `checks`, otherwise only the
        # test suite would be included
        checks = perSystem (system: self.flake.${system}.packages);
      };
}
