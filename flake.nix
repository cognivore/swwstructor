{
  description = "swwstructor — a single-tenant website constructor on the stickywebwm layout engine.";

  inputs = {
    # The engine, pinned to its public GitHub repo (flake.lock locks the exact
    # rev, so builds are reproducible and the repo is clone-and-build). nixpkgs
    # follows the engine's pin so the GHC and the whole Haskell package set are
    # shared and cached.
    stickywebwm.url = "github:cognivore/stickywebwm";
    nixpkgs.follows = "stickywebwm/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, stickywebwm, flake-utils }:
    let
      # Build the engine library (checked-in cabal2nix, no IFD) and the
      # swwstructor server against a given haskellPackages set.
      mkSww =
        { haskellPackages, lib }:
        let
          engine = haskellPackages.callPackage ./nix/deps/stickywebwm.nix {
            src = stickywebwm;
          };
          swwstructor-server = haskellPackages.callPackage ./nix/swwstructor.nix {
            stickywebwm = engine;
            src = lib.cleanSource ./.;
          };
        in
        { inherit swwstructor-server; };

      # The x86_64-linux deploy artifact, evaluable from any host.
      linuxPkgs = nixpkgs.legacyPackages.x86_64-linux;
      swwLinux = mkSww {
        haskellPackages = linuxPkgs.haskellPackages;
        lib = nixpkgs.lib;
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        swwNative = mkSww { haskellPackages = pkgs.haskellPackages; inherit lib; };

        # A GHC with the library's offline deps (containers) for the dev/test
        # loop that runs the pure test-suite against the engine source directly.
        ghc = pkgs.haskellPackages.ghcWithPackages (p: [ p.containers ]);

        # The clone-and-run launcher (`nix run .#run`): source secrets from
        # rageveil (the operator's tool, expected on PATH) into the environment
        # and exec the server. Light — only the server + this thin wrapper;
        # rageveil is NOT in the closure. All entries are optional; the server
        # falls back to an ephemeral key + a generated admin password.
        sww-run = pkgs.writeShellApplication {
          name = "sww-run";
          runtimeInputs = [ swwNative.swwstructor-server ];
          text = ''
            export SWW_SITE_DIR="''${SWW_SITE_DIR:-sites/nyt}"
            export PORT="''${PORT:-3000}"
            export BASE_URL="''${BASE_URL:-http://localhost:''${PORT}}"
            if command -v rageveil >/dev/null 2>&1; then
              v=$(rageveil show swwstructor/master 2>/dev/null || true);          if [ -n "$v" ]; then export SWW_MASTER_KEY="$v"; fi
              v=$(rageveil show swwstructor/admin 2>/dev/null || true);           if [ -n "$v" ]; then export SWW_ADMIN_PASSWORD="$v"; fi
              v=$(rageveil show swwstructor/stripe/pk 2>/dev/null || true);       if [ -n "$v" ]; then export SWW_STRIPE_PK="$v"; fi
              v=$(rageveil show swwstructor/stripe/sk 2>/dev/null || true);       if [ -n "$v" ]; then export SWW_STRIPE_SK="$v"; fi
              v=$(rageveil show swwstructor/stripe/webhook 2>/dev/null || true);  if [ -n "$v" ]; then export SWW_STRIPE_WEBHOOK="$v"; fi
            else
              echo "[run] rageveil not on PATH — using an ephemeral key + a generated admin password" >&2
            fi
            echo "[run] http://localhost:''${PORT}   admin: http://localhost:''${PORT}/admin" >&2
            exec swwstructor-server
          '';
        };
      in
      {
        packages = {
          swwstructor-server = swwNative.swwstructor-server;
          default = swwNative.swwstructor-server;
          # Exposed on every system so a Darwin host builds the deploy artifact
          # via a remote x86_64-linux builder: `.#swwstructor-server-x86_64-linux`.
          swwstructor-server-x86_64-linux = swwLinux.swwstructor-server;
        };

        # `nix run .#run` — clone-and-run, secrets from rageveil.
        apps.run = {
          type = "app";
          program = "${sww-run}/bin/sww-run";
        };
        apps.default = {
          type = "app";
          program = "${sww-run}/bin/sww-run";
        };

        devShells.default = pkgs.mkShell {
          packages = [
            ghc
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.fourmolu
            pkgs.nodejs_22
            pkgs.typescript
            pkgs.prettier
            pkgs.rust-script
            pkgs.age
            pkgs.just
            pkgs.awscli2
            pkgs.openssh
            pkgs.jq
          ];
        };
      }
    )
    // {
      # MULTI-INSTANCE NixOS module: several single-tenant sites on one box.
      nixosModules.swwstructor = import ./nix/module.nix self;
      nixosModules.default = import ./nix/module.nix self;
    };
}
