# The swwstructor NixOS module — MULTI-INSTANCE. One NixOS box runs several
# single-tenant sites: each `services.swwstructor.instances.<name>` becomes a
# systemd unit (with its own port, content dir, and age-decrypted secrets) plus
# a Caddy virtual host. Imported from the flake as `nixosModules.swwstructor`,
# parameterised by `self` so it can reach the built server package.
self:
{ config, lib, pkgs, ... }:

let
  cfg = config.services.swwstructor;
  defaultPkg = self.packages.${pkgs.stdenv.hostPlatform.system}.swwstructor-server;

  instanceModule =
    { name, ... }:
    {
      options = {
        domain = lib.mkOption {
          type = lib.types.str;
          description = "Public domain for this site (Caddy serves it, ACME issues a cert).";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Loopback port this instance listens on (Caddy reverse-proxies to it).";
        };
        siteDir = lib.mkOption {
          type = lib.types.path;
          description = "Content directory holding site.json/site.yaml (copied into the store).";
        };
        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/swwstructor/${name}";
          description = "Writable directory for the encrypted secret store (secrets.enc).";
        };
        masterKeyFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file (encrypted to the host ssh key) holding the 32-byte hex master key. If null, an ephemeral key is used (secrets won't persist).";
        };
        adminPasswordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the admin password. If null, a random one is generated at boot and logged.";
        };
        stage = lib.mkOption {
          type = lib.types.enum [ "test" "prod" ];
          default = "test";
          description = "Stripe stage label (test|prod).";
        };
        enableCaddy = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to add a Caddy virtual host for this instance's domain.";
        };
      };
    };
in
{
  options.services.swwstructor = {
    package = lib.mkOption {
      type = lib.types.package;
      default = defaultPkg;
      description = "The swwstructor-server package to run.";
    };
    acmeEmail = lib.mkOption {
      type = lib.types.str;
      default = "admin@example.com";
      description = "Contact e-mail for Caddy/ACME certificate issuance.";
    };
    instances = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule instanceModule);
      default = { };
      description = "The single-tenant sites to run on this host.";
    };
  };

  config = lib.mkIf (cfg.instances != { }) {
    systemd.services = lib.mapAttrs' (
      name: inst:
      lib.nameValuePair "swwstructor-${name}" {
        description = "swwstructor site: ${inst.domain}";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        path = [ pkgs.age ];
        serviceConfig = {
          Restart = "always";
          RestartSec = 3;
          User = "root";
          StateDirectory = "swwstructor/${name}";
        };
        environment = {
          PORT = toString inst.port;
          BASE_URL = "https://${inst.domain}";
          SWW_SITE_DIR = "${inst.siteDir}";
          SWW_DATA_DIR = inst.dataDir;
          SWW_STAGE = inst.stage;
          # CA bundle for the outbound Stripe API TLS call.
          SYSTEM_CERTIFICATE_PATH = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        };
        # Secrets are age-decrypted with THIS host's ssh key at start — never in
        # the Nix store in plaintext, never in the unit's static environment.
        script = ''
          set -eu
          umask 077
          mkdir -p ${inst.dataDir}
          hostkey=/etc/ssh/ssh_host_ed25519_key
        ''
        + lib.optionalString (inst.masterKeyFile != null) ''
          SWW_MASTER_KEY="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.masterKeyFile})"
          export SWW_MASTER_KEY
        ''
        + lib.optionalString (inst.adminPasswordFile != null) ''
          SWW_ADMIN_PASSWORD="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.adminPasswordFile})"
          export SWW_ADMIN_PASSWORD
        ''
        + ''
          exec ${cfg.package}/bin/swwstructor-server
        '';
      }
    ) cfg.instances;

    services.caddy = lib.mkIf (lib.any (i: i.enableCaddy) (lib.attrValues cfg.instances)) {
      enable = true;
      email = cfg.acmeEmail;
      virtualHosts = lib.mapAttrs' (
        name: inst:
        lib.nameValuePair inst.domain {
          extraConfig = "reverse_proxy localhost:${toString inst.port}";
        }
      ) (lib.filterAttrs (_: i: i.enableCaddy) cfg.instances);
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
