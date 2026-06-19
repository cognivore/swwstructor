{
  # NixOS host configuration for a swwstructor demo box on AWS EC2. A SEPARATE
  # flake from the app (they evolve independently). It runs single-tenant sites
  # via the module — each instance its own systemd unit + Caddy vhost (add more
  # for the multi-instance design). The x86_64-linux closure is built on a
  # builder and shipped (the box never compiles: max-jobs = 0).
  description = "swwstructor demo host — several single-tenant sites on one NixOS box";

  inputs = {
    # git+file so only tracked files are copied; `${app}/sites/*` are the content
    # dirs, available to the host as store paths.
    app.url = "git+file:///Users/sweater/Github/swwstructor";
    nixpkgs.follows = "app/nixpkgs";
  };

  outputs =
    { self, nixpkgs, app, ... }:
    let
      # aarch64-linux: built on the reliable local aarch64 linux-builder and run
      # on a Graviton t4g instance (a web demo is arch-agnostic).
      system = "aarch64-linux";
      # The operator's SSH public key (replace with your own before shipping).
      myKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC2CPbR76pncAZ3GKtS2HkzxOPMYQuJ823s8EBXqBAHM";
    in
    {
      nixosConfigurations.swwstructor = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          app.nixosModules.swwstructor
          (
            { modulesPath, pkgs, lib, ... }:
            {
              imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

              networking.hostName = "swwstructor";
              networking.firewall.allowedTCPPorts = [ 22 80 443 ];

              services.openssh.enable = true;
              services.openssh.settings.PermitRootLogin = "prohibit-password";
              users.users.root.openssh.authorizedKeys.keys = [ myKey ];

              # Several single-tenant sites on this one box. Point a DNS A-record
              # for each domain at the box; Caddy + ACME do the rest. For real
              # secrets, generate per-instance age files with `just site-new` and
              # set masterKeyFile/adminPasswordFile (here they are null → an
              # ephemeral key + a logged dev admin password).
              # Several single-tenant sites can run here; this live demo serves
              # one (nyt.fere.me). Add more by adding instances with real
              # subdomains + DNS A-records.
              services.swwstructor = {
                acmeEmail = "jm@memorici.de";
                instances = {
                  nyt = {
                    domain = "nyt.fere.me";
                    port = 4001;
                    siteDir = app + "/sites/nyt";
                    stage = "test";
                    # Secrets age-encrypted to THIS box's ssh host key, decrypted
                    # into the unit env at boot — never plaintext in the store.
                    masterKeyFile = ./secrets/nyt-master.age;
                    adminPasswordFile = ./secrets/nyt-admin.age;
                  };
                };
              };

              # This box only RUNS prebuilt closures — never compiles.
              nix.settings.max-jobs = 0;
              system.stateVersion = "26.05";
            }
          )
        ];
      };
    };
}
