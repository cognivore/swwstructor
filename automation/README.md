# swwstructor automation

The automation layer for `swwstructor` is written in **rust-script**, not shell
(CTO mandate). Each script has a `#!/usr/bin/env rust-script` shebang and an
embedded cargo manifest (a `//! ```cargo … ``` ` block). They are "Rust instead
of bash": typed structs for the JSON the CLIs emit, `Result`-based error
handling, idempotent find-or-create, and JSON state files — all the *logic* in
Rust, the *heavy lifting* delegated to the same battle-tested CLIs (`aws`, `nix`,
`ssh`, `age`) via `std::process::Command`.

| Script          | Does                                                                    |
| --------------- | ----------------------------------------------------------------------- |
| `provision.rs`  | idempotent AWS EC2 NixOS provisioner (find-or-create by `Name` tag)     |
| `ship.rs`       | build the system closure, copy it to the box, activate it               |
| `site.rs`       | scaffold a new single-tenant site instance (content + secrets + snippet)|

Each script is self-documenting: `rust-script automation/<script>.rs --help`.

## Installing rust-script

`rust-script` compiles the script (and its declared deps) with `cargo`/`rustc`
on first run and caches the build, so a Rust toolchain must also be on PATH.

- **Dev shell** — it is provided by the project dev shell; `nix develop` (or
  `direnv`) puts `rust-script`, `cargo`, and `rustc` on PATH.
- **Ad hoc** — `nix shell nixpkgs#rust-script nixpkgs#cargo nixpkgs#rustc`.
- **Via just** — the recipes below assume `rust-script` is on PATH (e.g. you are
  inside the dev shell).

The first run of each script downloads/builds its deps (`serde`, `serde_json`);
subsequent runs are instant from cache.

You also need the delegated CLIs on PATH for the steps that use them: `aws`
(provision), `nix` + `ssh` (ship), and `age` (site secrets). These are in the
dev shell too.

## State files

`provision.rs` writes `.build/swwstructor-state/<name>.json`:

```json
{
  "name": "swwstructor",
  "instanceId": "i-0123…",
  "publicIp": "203.0.113.7",
  "privateIp": "10.0.1.23",
  "region": "eu-west-2",
  "instanceType": "t3.small",
  "ami": "ami-009a6dc6bf97a4da7",
  "provisionedAt": "1750000000"
}
```

`ship.rs` reads `publicIp` from this file (keyed by `--name`) to find the box.
`.build/` is git-ignored; state is per-operator/per-machine.

## End-to-end runbook

A new single-tenant site, from nothing to live, is five steps.

### 1. Scaffold the site — `just site-new`

```sh
just site-new shop shop.example.com 8081
```

This creates `sites/shop/site.yaml` (a minimal masthead + nav + prose starter,
valid for the constructor's content schema), generates a 32-byte hex master key
and a random admin password, and prints the
`services.swwstructor.instances.shop = { … };` snippet to paste into
`deploy/flake.nix`.

- **Without `--host-key`** the secrets are written (mode `0600`) to
  `.build/swwstructor-state/shop-secrets.txt` with a warning — you **must**
  age-encrypt them to `deploy/secrets/shop-*.age` before deploying.
- **With `--host-key`** (the box's `ssh_host_ed25519_key.pub` line, or an age
  recipient, or a path to a file containing one) the secrets are encrypted
  straight to `deploy/secrets/shop-master.age` and `…-admin.age`:

  ```sh
  just site-new shop shop.example.com 8081 --host-key ./shop-host.pub
  ```

Secret *values* are never printed to stdout — only the paths written.

Paste the printed snippet into `deploy/flake.nix` (under
`services.swwstructor.instances`) and commit your `sites/shop/` content and
`deploy/secrets/*.age`.

### 2. Provision the box — `just provision`

```sh
just provision
```

Find-or-creates one EC2 instance tagged `Name=swwstructor` from the official
NixOS x86_64 AMI in `eu-west-2`, ensures the SSH key pair and a security group
(TCP 22/80/443), waits for it to be `running`, and writes the state file.
Idempotent: re-running reuses the existing instance.

Overrides pass through, e.g.:

```sh
just provision --name swwstructor --region eu-west-2 --type t3.small --ami ami-009a6dc6bf97a4da7
```

The SSH public key is read from `$SWW_SSH_PUBKEY` or `~/.ssh/id_ed25519.pub` and
imported as the key pair if absent. Requires working `aws` credentials
(`aws sts get-caller-identity` must succeed).

### 3. First deploy — `just ship-boot`

```sh
just ship-boot
```

Builds the system closure on your nix builders (the box has
`nix.settings.max-jobs = 0` and never compiles), copies the closure to
`root@<ip>`, and activates it in `boot` mode. **Reboot the box afterwards** so
it comes up on the new system + bootloader entry.

### 4. DNS — point the domain at the box

Create an **A record** for the site's domain (e.g. `shop.example.com`) pointing
at the box's `publicIp` (see `.build/swwstructor-state/<name>.json`, or the
provision output). Once it resolves and TLS provisions, the site is reachable.

### 5. Subsequent deploys — `just ship`

```sh
just ship              # mode defaults to `switch` — activate now
just ship dry-activate # preview what switching would change
```

After the first `boot` + reboot, use `switch` (the default) for every later
deploy: it activates the new closure immediately without a reboot. Add or edit
a site by repeating step 1, committing, then `just ship`.

## Multi-tenant note

One NixOS box runs several site instances. Each `just site-new` adds another
`services.swwstructor.instances.<slug>` entry (its own domain, port, content
dir, and secrets). Provision once; ship after each content/config change.

## Verification

```sh
just matrix     # server build + Haskell tests + TS checks + automation --help smokes
```

Individual checks: `just build-server`, `just test`, `just ts-check`. The
`--help` smokes prove each rust-script compiles.
