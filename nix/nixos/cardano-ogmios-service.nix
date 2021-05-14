{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption types elem optionalString optionalAttrs;
  cfg = config.services.cardano-ogmios;
  genesis = builtins.fromJSON (builtins.readFile cfg.environment.networkConfig.ShelleyGenesisFile);
in {
  options = {
    services.cardano-ogmios = {
      enable = mkEnableOption "enable the cardano-ogmios service";
      script = mkOption {
        internal = true;
        type = types.package;
      };
      environment = mkOption {
        type = types.nullOr types.attrs;
        default = cfg.ogmiosPkgs.cardanoLib.environments.${cfg.network} or null;
      };
      network = mkOption {
        type = types.nullOr types.str;
        description = "network name";
      };
      networkMagic = mkOption {
        type = types.int;
        default = genesis.networkMagic;
        description = "network magic";
      };
      networkSystemStart = mkOption {
        type = types.str;
        default = genesis.systemStart;
        description = "network system start (shelley genesis systemStart timestamp)";
      };
      networkSlotsPerEpoch = mkOption {
        type = types.nullOr types.int;
        default = if cfg.environment != null
          then genesis.epochLength / genesis.slotLength
          else null;
        description = "network slots per epoch";
      };
      nodeSocketPath = mkOption {
        type = types.nullOr (types.either types.str types.path);
        description = "Path to cardano-node socket";
        default = null;
      };
      port = mkOption {
        type = types.int;
        default = 1337;
        description = ''
          The port number
        '';
      };
      hostAddr = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host address to bind to
        '';
      };
      timeout = mkOption {
        type = types.int;
        default = 90;
        description = ''
          Number of seconds of inactivity after which the
          server should close client connections.
        '';
      };
      logLevel = lib.mkOption {
        type = types.enum ["Debug" "Info" "Notice" "Warning" "Error" "Critical" "Alert" "Emergency"];
        default = "Info";
      };
      ogmiosPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. {};
        defaultText = "cardano-db-sync pkgs";
        description = ''
          The cardano-db-sync packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.ogmiosPkgs.ogmios;
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-ogmios.script = let
      network = "OGMIOS_NETWORK=${if (elem cfg.network ["mainnet" "testnet" "staging"])
        then cfg.network
        else ''${toString cfg.networkMagic}:$(date +\%s -d "${cfg.networkSystemStart}")${optionalString (cfg.networkSlotsPerEpoch != null) ":${toString cfg.networkSlotsPerEpoch}"}''
      }";
      cmd = ''
        ${cfg.package}/bin/ogmios \
        --node-socket "$CARDANO_NODE_SOCKET_PATH" \
        --host ${cfg.hostAddr} \
        --port ${toString cfg.port} \
        --timeout ${toString cfg.timeout} \
        --log-level ${toString cfg.logLevel}
      '';
    in pkgs.writeShellScript "cardano-db-sync" ''
      ${if (cfg.nodeSocketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export CARDANO_NODE_SOCKET_PATH=\"${cfg.nodeSocketPath}\""}
      echo
      echo "Launching ${network} ${cmd}"
      echo
      ${network} exec ${cmd}
    '';
    systemd.services.cardano-ogmios = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart   = cfg.script;
        DynamicUser = true;
      };
    };
  };
}
