{ pkgs
, customConfigs ? [ pkgs.customConfig ] }:
let
  inherit (pkgs) lib cardanoLib;
  inherit (pkgs.commonLib) evalService;
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-ogmios";
      modules = [
        ./nixos/cardano-ogmios-service.nix
        {
          services.cardano-ogmios = {
            network = lib.mkDefault envConfig.name;
            ogmiosPkgs = lib.mkDefault pkgs;
          };
        }
      ];
    };
  in lib.recurseIntoAttrs {
    ogmios = pkgs.writeScriptBin "ogmios-${service.network}" ''
      #!${pkgs.runtimeShell}
      set -euo pipefail
      ${service.script} $@
    '';
  };
in cardanoLib.forEnvironments mkScript
