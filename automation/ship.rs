#!/usr/bin/env rust-script
//! Build the swwstructor system closure, copy it to the box, and activate it.
//!
//! "Rust instead of bash": typed state, explicit modes, fail-loud streaming.
//! The build, copy, and activation are delegated to `nix` and `ssh` exactly per
//! the deploy contract:
//!
//!   nix build ./deploy#nixosConfigurations.swwstructor.config.system.build.toplevel \
//!     --out-link /tmp/swwstructor-system -L
//!   nix copy --to ssh://root@<ip> <closure> --no-check-sigs --substitute-on-destination
//!   ssh root@<ip> "nix-env -p /nix/var/nix/profiles/system --set <closure> \
//!     && <closure>/bin/switch-to-configuration <mode>"
//!
//! The box has nix.settings.max-jobs = 0 so it NEVER compiles: the build happens
//! on the local builder and only the realised closure is copied across. Use mode
//! `boot` for the first deploy (then reboot), `switch` thereafter, `dry-activate`
//! to preview.
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1", features = ["derive"] }
//! serde_json = "1"
//! ```
use serde::Deserialize;
use std::path::PathBuf;
use std::process::Command;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const LOG_PREFIX: &str = "[ship]";

fn log(msg: &str) {
    eprintln!("{LOG_PREFIX} {msg}");
}
fn ok(msg: &str) {
    eprintln!("{LOG_PREFIX} [ok] {msg}");
}
fn fail<T>(msg: impl Into<String>) -> Result<T> {
    Err(msg.into().into())
}

// ── Deploy contract constants ───────────────────────────────────────────────
const DEFAULT_NAME: &str = "swwstructor";
/// The flake attr that builds the whole system (its toplevel derivation).
const FLAKE_ATTR: &str =
    "./deploy#nixosConfigurations.swwstructor.config.system.build.toplevel";
const OUT_LINK: &str = "/tmp/swwstructor-system";

// ── The single command helper (capturing) ──────────────────────────────────
/// Run `cmd args...`, capturing stdout; error (with stderr) on a non-zero exit
/// or spawn failure. Used where we need the output (e.g. `readlink`).
fn run(cmd: &str, args: &[&str]) -> Result<String> {
    let output = Command::new(cmd)
        .args(args)
        .output()
        .map_err(|e| format!("failed to spawn `{cmd}` (is it on PATH?): {e}"))?;
    if !output.status.success() {
        let code = output
            .status
            .code()
            .map(|c| c.to_string())
            .unwrap_or_else(|| "signal".into());
        let stderr = String::from_utf8_lossy(&output.stderr);
        return fail(format!(
            "`{cmd} {}` exited {code}:\n{}",
            args.join(" "),
            stderr.trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// Run `cmd args...` inheriting stdout/stderr so long builds and copies STREAM
/// live to the terminal. Fails loudly on any non-zero exit (the deploy contract:
/// any failed step aborts the ship).
fn run_streaming(cmd: &str, args: &[&str]) -> Result<()> {
    log(&format!("$ {cmd} {}", args.join(" ")));
    let status = Command::new(cmd)
        .args(args)
        .status()
        .map_err(|e| format!("failed to spawn `{cmd}` (is it on PATH?): {e}"))?;
    if !status.success() {
        let code = status
            .code()
            .map(|c| c.to_string())
            .unwrap_or_else(|| "signal".into());
        return fail(format!("`{cmd} {}` exited {code}", args.join(" ")));
    }
    Ok(())
}

fn run_ok(cmd: &str, args: &[&str]) -> bool {
    Command::new(cmd)
        .args(args)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// ── State file (written by provision.rs) ────────────────────────────────────
#[derive(Deserialize)]
struct State {
    #[serde(rename = "publicIp")]
    public_ip: String,
}

fn state_path(name: &str) -> PathBuf {
    PathBuf::from(".build/swwstructor-state").join(format!("{name}.json"))
}

/// Read the target box's public IP from the provisioner's state file.
fn ip_from_state(name: &str) -> Result<String> {
    let path = state_path(name);
    let raw = std::fs::read_to_string(&path).map_err(|e| {
        format!(
            "cannot read state file {} ({e}). Run `./automation/provision.rs --name {name}` first, or pass --ip.",
            path.display()
        )
    })?;
    let state: State = serde_json::from_str(&raw)
        .map_err(|e| format!("parsing {}: {e}", path.display()))?;
    if state.public_ip.trim().is_empty() {
        return fail(format!(
            "state file {} has an empty publicIp; re-run provision.rs",
            path.display()
        ));
    }
    Ok(state.public_ip)
}

// ── Modes ───────────────────────────────────────────────────────────────────
#[derive(Clone, Copy)]
enum Mode {
    Boot,
    Switch,
    DryActivate,
}

impl Mode {
    /// The literal switch-to-configuration verb.
    fn as_str(self) -> &'static str {
        match self {
            Mode::Boot => "boot",
            Mode::Switch => "switch",
            Mode::DryActivate => "dry-activate",
        }
    }
    fn parse(s: &str) -> Result<Mode> {
        match s {
            "boot" => Ok(Mode::Boot),
            "switch" => Ok(Mode::Switch),
            "dry-activate" => Ok(Mode::DryActivate),
            other => fail(format!(
                "unknown mode `{other}` (expected boot | switch | dry-activate)"
            )),
        }
    }
}

// ── Config from args ────────────────────────────────────────────────────────
struct Config {
    /// Either a state-file name to resolve, or None when --ip is given directly.
    name: Option<String>,
    ip: Option<String>,
    mode: Mode,
}

const USAGE: &str = "\
ship.rs — build + copy + activate the swwstructor system closure

USAGE:
    rust-script automation/ship.rs [OPTIONS] [MODE]

MODE (positional):
    boot           install the bootloader entry, activate on next reboot (first deploy)
    switch         activate now and make default            (default)
    dry-activate   show what switching would do; change nothing

OPTIONS:
    --name <host>  resolve the target IP from .build/swwstructor-state/<host>.json
                   (default: swwstructor, unless --ip is given)
    --ip <ip>      target the box at this IP directly (skips the state file)
    -h, --help     print this help and exit

The build runs locally (or on your configured nix builders) and only the
realised closure is copied to root@<ip>; the box never compiles.";

fn parse_args(argv: &[String]) -> Result<Config> {
    let mut name: Option<String> = None;
    let mut ip: Option<String> = None;
    let mut mode: Option<Mode> = None;
    let mut i = 0;
    while i < argv.len() {
        let arg = argv[i].as_str();
        match arg {
            "--name" => {
                i += 1;
                name = Some(
                    argv.get(i)
                        .cloned()
                        .ok_or("option `--name` requires a value")?,
                );
            }
            "--ip" => {
                i += 1;
                ip = Some(
                    argv.get(i)
                        .cloned()
                        .ok_or("option `--ip` requires a value")?,
                );
            }
            "-h" | "--help" => {
                println!("{USAGE}");
                std::process::exit(0);
            }
            other if other.starts_with('-') => {
                return fail(format!("unknown argument: {other}\n\n{USAGE}"));
            }
            other => {
                // The sole positional is the mode; reject a second one.
                if mode.is_some() {
                    return fail(format!("unexpected extra positional argument: {other}"));
                }
                mode = Some(Mode::parse(other)?);
            }
        }
        i += 1;
    }
    // Default to the canonical host name only when no --ip was supplied.
    if name.is_none() && ip.is_none() {
        name = Some(DEFAULT_NAME.into());
    }
    Ok(Config {
        name,
        ip,
        mode: mode.unwrap_or(Mode::Switch),
    })
}

// ── Build / copy / activate ─────────────────────────────────────────────────
/// Build the system closure and return the realised store path. `-L` streams
/// full build logs; `--out-link` gives us a stable symlink to resolve.
fn build_closure() -> Result<String> {
    log("building x86_64-linux system closure (this runs on your nix builders)");
    run_streaming("nix", &["build", FLAKE_ATTR, "--out-link", OUT_LINK, "-L"])?;
    // Resolve the symlink to the concrete /nix/store path we copy + set.
    let sys = run("readlink", &["-f", OUT_LINK])?.trim().to_string();
    if sys.is_empty() {
        return fail(format!("could not resolve {OUT_LINK} to a store path"));
    }
    ok(&format!("system = {sys}"));
    Ok(sys)
}

/// Copy the closure to the box over SSH. `--no-check-sigs` (we trust our own
/// builder) and `--substitute-on-destination` (let the box pull what it can from
/// caches, copying only the rest).
fn copy_closure(target: &str, closure: &str) -> Result<()> {
    log(&format!("copying closure to {target}"));
    let dest = format!("ssh://{target}");
    run_streaming(
        "nix",
        &[
            "copy",
            "--to",
            &dest,
            closure,
            "--no-check-sigs",
            "--substitute-on-destination",
        ],
    )
}

/// Set the system profile to the new closure and run switch-to-configuration in
/// the requested mode, over a single SSH invocation (one round trip, atomic).
fn activate(target: &str, closure: &str, mode: Mode) -> Result<()> {
    log(&format!("activating ({}) on {target}", mode.as_str()));
    // For dry-activate, do NOT repoint the system profile — just preview.
    let remote = if matches!(mode, Mode::DryActivate) {
        format!("'{closure}/bin/switch-to-configuration' {}", mode.as_str())
    } else {
        format!(
            "nix-env -p /nix/var/nix/profiles/system --set '{closure}' && '{closure}/bin/switch-to-configuration' {}",
            mode.as_str()
        )
    };
    run_streaming("ssh", &[target, &remote])
}

// ── main ────────────────────────────────────────────────────────────────────
fn main() {
    let argv: Vec<String> = std::env::args().skip(1).collect();
    if let Err(e) = real_main(&argv) {
        eprintln!("{LOG_PREFIX} ERROR: {e}");
        std::process::exit(1);
    }
}

fn real_main(argv: &[String]) -> Result<()> {
    let cfg = parse_args(argv)?;

    // nix and ssh are non-negotiable for shipping. Each tool gets its own
    // version flag: `nix --version` exits 0, but `ssh --version` is an illegal
    // option (255) — OpenSSH uses `-V`.
    for (tool, flag) in [("nix", "--version"), ("ssh", "-V")] {
        if !run_ok(tool, &[flag]) {
            return fail(format!(
                "required tool `{tool}` not found on PATH (try: nix develop)"
            ));
        }
    }

    // Resolve the target: an explicit --ip wins, else the state file.
    let ip = match &cfg.ip {
        Some(ip) => ip.clone(),
        None => {
            let name = cfg
                .name
                .as_deref()
                .ok_or("internal: no name and no ip (should be unreachable)")?;
            log(&format!("reading target IP from state file for '{name}'"));
            ip_from_state(name)?
        }
    };
    let target = format!("root@{ip}");
    log(&format!("target = {target}, mode = {}", cfg.mode.as_str()));

    let closure = build_closure()?;
    copy_closure(&target, &closure)?;
    activate(&target, &closure, cfg.mode)?;

    match cfg.mode {
        Mode::Boot => {
            ok("done (boot). Reboot the box, then use `switch` for subsequent deploys.");
        }
        Mode::Switch => ok("done (switch). The new system is live."),
        Mode::DryActivate => ok("done (dry-activate). No changes were applied."),
    }
    Ok(())
}
