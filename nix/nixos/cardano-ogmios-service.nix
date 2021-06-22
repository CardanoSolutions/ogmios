{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption types elem optionalString optionalAttrs;
  cfg = config.services.cardano-ogmios;
in {
  options = {
    services.cardano-ogmios = {
      enable = mkEnableOption "enable the cardano-ogmios service";
      script = mkOption {
        internal = true;
        type = types.package;
      };
      nodeConfig = mkOption {
        type = types.nullOr (types.either types.str types.path);
        description = "Path to cardano-node JSON/Yaml config file";
        default = null;
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
        defaultText = "ogmios pkgs";
        description = ''
          The ogmios packages and library that should be used.
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
      cmd = ''
        ${cfg.package}/bin/ogmios \
        --node-socket "$CARDANO_NODE_SOCKET_PATH" \
        --node-config ${toString cfg.nodeConfig} \
        --host ${cfg.hostAddr} \
        --port ${toString cfg.port} \
        --timeout ${toString cfg.timeout} \
        --log-level ${toString cfg.logLevel}
      '';
    in pkgs.writeShellScript "cardano-ogmios" ''
      ${if (cfg.nodeSocketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export CARDANO_NODE_SOCKET_PATH=\"${cfg.nodeSocketPath}\""}
      echo
      echo "Launching ${cmd}"
      echo
      exec ${cmd}
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
