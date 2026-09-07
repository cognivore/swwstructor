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
        stripePkFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Stripe publishable key. Seeds the encrypted store at boot ONLY while the store has no keys yet; once the owner saves keys via /admin, the store wins.";
        };
        stripeSkFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Stripe secret key (seed-only, see stripePkFile).";
        };
        stripeWebhookFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Stripe webhook signing secret (seed-only, see stripePkFile).";
        };
        stage = lib.mkOption {
          type = lib.types.enum [ "test" "prod" ];
          default = "test";
          description = "Stripe stage label (test|prod).";
        };
        googleClientIdFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Google OAuth client id (seed-only, like stripePkFile). Powers class-booking calendar invites + Gmail mail.";
        };
        googleClientSecretFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Google OAuth client secret (seed-only).";
        };
        googleRefreshTokenFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "age file holding the Google refresh token for the account the site acts as (seed-only).";
        };
        booking = {
          staff = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Addresses invited to every class-booking calendar event and copied on every booking mail.";
          };
          replyTo = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Reply-To on outgoing booking mail (e.g. the school's public address).";
          };
          name = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Short brand name used in booking mail copy (default: the site title).";
          };
          timeZone = lib.mkOption {
            type = lib.types.str;
            default = "Europe/London";
            description = "IANA time zone the classes run in.";
          };
          tzOffset = lib.mkOption {
            type = lib.types.str;
            default = "+01:00";
            description = "UTC offset of the class dates in that zone (no tz database in-process).";
          };
          venueNoun = lib.mkOption {
            type = lib.types.str;
            default = "venue";
            description = "What this site calls a venue in student-facing copy (e.g. \"kitchen\" for a pastry school).";
          };
        };
        analyticsScript = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          example = "https://plausible.example.com/js/plausible.js";
          description = ''
            Analytics tracker script to load on every PUBLIC page (the admin and
            its previews never carry it). It is loaded deferred with
            `data-domain` set to this instance's domain — the shape a
            self-hosted Plausible expects. null = no analytics.
          '';
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
        # age: secret decryption at start. imagemagick: HEIC -> JPEG for the
        # media library (iPhone uploads from browsers that cannot decode HEIC).
        path = [ pkgs.age pkgs.imagemagick ];
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
          # CA bundle for the outbound Stripe/Google API TLS calls.
          SYSTEM_CERTIFICATE_PATH = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          SWW_BOOKING_TZ = inst.booking.timeZone;
          SWW_BOOKING_TZ_OFFSET = inst.booking.tzOffset;
          SWW_BOOKING_VENUE_NOUN = inst.booking.venueNoun;
        }
        // lib.optionalAttrs (inst.booking.staff != [ ]) {
          SWW_BOOKING_STAFF = lib.concatStringsSep "," inst.booking.staff;
        }
        // lib.optionalAttrs (inst.booking.replyTo != null) {
          SWW_BOOKING_REPLY_TO = inst.booking.replyTo;
        }
        // lib.optionalAttrs (inst.booking.name != null) {
          SWW_BOOKING_NAME = inst.booking.name;
        }
        // lib.optionalAttrs (inst.analyticsScript != null) {
          SWW_ANALYTICS_SRC = inst.analyticsScript;
          SWW_ANALYTICS_DOMAIN = inst.domain;
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
        + lib.optionalString (inst.stripePkFile != null) ''
          SWW_STRIPE_PK="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.stripePkFile})"
          export SWW_STRIPE_PK
        ''
        + lib.optionalString (inst.stripeSkFile != null) ''
          SWW_STRIPE_SK="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.stripeSkFile})"
          export SWW_STRIPE_SK
        ''
        + lib.optionalString (inst.stripeWebhookFile != null) ''
          SWW_STRIPE_WEBHOOK="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.stripeWebhookFile})"
          export SWW_STRIPE_WEBHOOK
        ''
        + lib.optionalString (inst.googleClientIdFile != null) ''
          SWW_GOOGLE_CLIENT_ID="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.googleClientIdFile})"
          export SWW_GOOGLE_CLIENT_ID
        ''
        + lib.optionalString (inst.googleClientSecretFile != null) ''
          SWW_GOOGLE_CLIENT_SECRET="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.googleClientSecretFile})"
          export SWW_GOOGLE_CLIENT_SECRET
        ''
        + lib.optionalString (inst.googleRefreshTokenFile != null) ''
          SWW_GOOGLE_REFRESH_TOKEN="$(${pkgs.age}/bin/age -d -i "$hostkey" ${inst.googleRefreshTokenFile})"
          export SWW_GOOGLE_REFRESH_TOKEN
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
