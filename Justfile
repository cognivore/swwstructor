# swwstructor — task runner.
#
# Thin recipes that delegate to the rust-script automation in automation/*.rs
# and to the project's build/test commands. The AUTOMATION LOGIC lives in the
# .rs files (typed, Result-based, idempotent); these recipes are one-liners.
#
# Run `just` (no args) to list recipes. rust-script provides the scripts'
# shebang interpreter; get it via `nix shell nixpkgs#rust-script` or the dev
# shell (see automation/README.md).

# Show the recipe list when invoked with no arguments.
default:
    @just --list

# ── Run (clone-and-run) ──────────────────────────────────────────────────────

# Run the server locally, sourcing secrets from rageveil. The download-and-run
# entry point. Optional flags pass through: `just run --port 8080 --site sites/okashi`
run *ARGS:
    rust-script automation/run.rs {{ARGS}}

# ── Infra: provision / ship ─────────────────────────────────────────────────

# Extra flags pass through, e.g. `just provision --region eu-west-1 --type t3.medium`.
# find-or-create the AWS EC2 NixOS box; writes .build/swwstructor-state/<name>.json (idempotent)
provision *ARGS:
    rust-script automation/provision.rs {{ARGS}}

# build + copy + activate the system closure (MODE: switch [default] | boot | dry-activate)
ship MODE="switch" *ARGS:
    rust-script automation/ship.rs {{ARGS}} {{MODE}}

# first-deploy convenience: activate in `boot` mode (then reboot the box)
ship-boot *ARGS:
    rust-script automation/ship.rs {{ARGS}} boot

# scaffold a new single-tenant site instance (idempotent); prints the Nix snippet
# Pass --host-key to age-encrypt secrets: `just site-new shop shop.example.com 8081 --host-key ./host.pub`
site-new SLUG DOMAIN PORT *ARGS:
    rust-script automation/site.rs --slug {{SLUG}} --domain {{DOMAIN}} --port {{PORT}} {{ARGS}}

# ── Dev helpers: build / test ───────────────────────────────────────────────

# Build the Haskell server (the flake package the deploy config consumes).
build-server:
    nix build .#swwstructor-server

# Run the Haskell test suite against the local stickywebwm engine dev shell.
test:
    nix develop /Users/sweater/Github/stickywebwm -c ghc -XGHC2021 -isrc -i/Users/sweater/Github/stickywebwm/src -outputdir /tmp/sww-build -o /tmp/sww-spec test/Spec.hs && /tmp/sww-spec

# Typecheck + test the TypeScript client.
ts-check:
    cd ts && npx tsc --noEmit && node --test

# ── Verification matrix ──────────────────────────────────────────────────────

# run the full verification matrix: server build, Haskell + TS tests, automation --help smokes
matrix:
    @echo "==> [1/6] build-server"
    just build-server
    @echo "==> [2/6] test (haskell)"
    just test
    @echo "==> [3/6] ts-check"
    just ts-check
    @echo "==> [4/6] provision.rs --help"
    rust-script automation/provision.rs --help
    @echo "==> [5/6] ship.rs --help"
    rust-script automation/ship.rs --help
    @echo "==> [6/6] site.rs --help"
    rust-script automation/site.rs --help
    @echo "==> matrix OK"
