{
  description = "cardano-ogmios";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    customConfig = {
      url = "path:./custom-config";
    };
  };

  outputs = { self, iohkNix, haskellNix, nixpkgs, utils, customConfig, ... }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) systems mapAttrs recursiveUpdate mkDefault optionalAttrs nameValuePair
        attrNames getAttrs head;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          customConfig = recursiveUpdate
            (import ./custom-config final.customConfig)
            customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib // iohkNix.lib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.ogmiosProject.flake {};

        scripts = flattenTree pkgs.scripts;

        checkNames = attrNames flake.checks;

        exes = collectExes flake.packages;

        packages = {
          inherit (pkgs) ;
        }
        // (collectExes flake.packages)
        // scripts
        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" flake.checks);

      in recursiveUpdate flake {

        inherit packages;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."ogmios:exe:ogmios";

        # Run by `nix run .`
        defaultApp = flake.apps."ogmios:exe:ogmios";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
        } # nix run .#<exe>
        // (collectExes flake.apps);

      }
    ) // {
      overlay = final: prev: with self.legacyPackages.${final.system}; {
        inherit ogmios;
      };
      nixosModules = {
        cardano-ogmios = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-ogmios-service.nix ];
          services.cardano-ogmios.ogmiosPkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
