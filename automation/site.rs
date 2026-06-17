#!/usr/bin/env rust-script
//! Scaffold a new single-tenant swwstructor site instance.
//!
//! "Rust instead of bash": idempotent find-or-create (never clobbers an existing
//! file), typed flow, std-only crypto-material generation (32 bytes from
//! /dev/urandom, hex-encoded — no crate), and `age` for at-rest secrets.
//!
//! One site = one content dir (`sites/<slug>/site.yaml`) + its own port + domain
//! + secrets. ONE NixOS box runs SEVERAL instances via
//! `services.swwstructor.instances.<slug> = { domain, port, siteDir, ... }`.
//! This script writes the content starter + the secrets, and prints the exact
//! Nix snippet to paste into deploy/flake.nix.
//!
//! Secrets are NEVER printed to stdout. If a `--host-key` recipient is given we
//! `age -r <recipient>` straight into deploy/secrets/<slug>-*.age; otherwise we
//! write them (0600) to .build/swwstructor-state/<slug>-secrets.txt with a loud
//! warning that they must be age-encrypted before deploy.
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1", features = ["derive"] }
//! serde_json = "1"
//! ```
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const LOG_PREFIX: &str = "[site]";

fn log(msg: &str) {
    eprintln!("{LOG_PREFIX} {msg}");
}
fn ok(msg: &str) {
    eprintln!("{LOG_PREFIX} [ok] {msg}");
}
fn warn(msg: &str) {
    eprintln!("{LOG_PREFIX} [warn] {msg}");
}
fn fail<T>(msg: impl Into<String>) -> Result<T> {
    Err(msg.into().into())
}

// ── Config from args ────────────────────────────────────────────────────────
struct Config {
    slug: String,
    domain: String,
    port: u16,
    /// An age recipient string (e.g. `ssh-ed25519 AAAA...` from the box's
    /// ssh_host_ed25519_key.pub) or a path to a file containing one.
    host_key: Option<String>,
}

const USAGE: &str = "\
site.rs — scaffold a new single-tenant swwstructor site instance

USAGE:
    rust-script automation/site.rs --slug <slug> --domain <domain> --port <n> [--host-key <recipient|path>]

OPTIONS:
    --slug <slug>          site id; content lands in sites/<slug>/        (required)
    --domain <domain>      public domain, e.g. shop.example.com           (required)
    --port <n>             local port this instance listens on            (required)
    --host-key <r|path>    age recipient (the box's ssh_host_ed25519_key.pub line,
                           or a path to a file containing one). If given, secrets
                           are encrypted to deploy/secrets/<slug>-*.age. If omitted,
                           plaintext secrets are written to .build/ with a warning.
    -h, --help             print this help and exit

Idempotent: existing files are never overwritten. Prints the
services.swwstructor.instances.<slug> Nix snippet to paste into deploy/flake.nix.";

fn parse_args(argv: &[String]) -> Result<Config> {
    let mut slug: Option<String> = None;
    let mut domain: Option<String> = None;
    let mut port: Option<u16> = None;
    let mut host_key: Option<String> = None;
    let mut i = 0;
    while i < argv.len() {
        let arg = argv[i].as_str();
        // Small helper to grab the next token as this option's value.
        let value = |i: &mut usize| -> Result<String> {
            *i += 1;
            argv.get(*i)
                .cloned()
                .ok_or_else(|| format!("option `{arg}` requires a value").into())
        };
        match arg {
            "--slug" => slug = Some(value(&mut i)?),
            "--domain" => domain = Some(value(&mut i)?),
            "--port" => {
                let raw = value(&mut i)?;
                port = Some(
                    raw.parse::<u16>()
                        .map_err(|_| format!("--port must be a number in 1..=65535 (got `{raw}`)"))?,
                );
            }
            "--host-key" => host_key = Some(value(&mut i)?),
            "-h" | "--help" => {
                println!("{USAGE}");
                std::process::exit(0);
            }
            other => return fail(format!("unknown argument: {other}\n\n{USAGE}")),
        }
        i += 1;
    }
    let slug = slug.ok_or_else(|| format!("--slug is required\n\n{USAGE}"))?;
    validate_slug(&slug)?;
    let domain = domain.ok_or_else(|| format!("--domain is required\n\n{USAGE}"))?;
    let port = port.ok_or_else(|| format!("--port is required\n\n{USAGE}"))?;
    if port == 0 {
        return fail("--port must be in 1..=65535");
    }
    Ok(Config {
        slug,
        domain,
        port,
        host_key,
    })
}

/// A slug becomes a path segment AND a Nix attribute name, so constrain it to a
/// safe, lowercase, identifier-ish shape up front.
fn validate_slug(slug: &str) -> Result<()> {
    if slug.is_empty() {
        return fail("--slug must not be empty");
    }
    let first_ok = slug
        .chars()
        .next()
        .map(|c| c.is_ascii_lowercase())
        .unwrap_or(false);
    let rest_ok = slug
        .chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
    if !first_ok || !rest_ok {
        return fail(format!(
            "--slug `{slug}` invalid: use lowercase letters, digits, and '-', starting with a letter"
        ));
    }
    Ok(())
}

// ── std-only secret material ────────────────────────────────────────────────
/// Read `n` bytes of OS entropy from /dev/urandom and hex-encode them. std-only:
/// no `rand`/`hex` crate. /dev/urandom is the right source on the macOS/Linux
/// dev + CI boxes this runs on. `read_exact` reads exactly `n` bytes rather than
/// slurping the (infinite) device.
fn random_hex(n: usize) -> Result<String> {
    use std::io::Read;
    let mut f = std::fs::File::open("/dev/urandom")
        .map_err(|e| format!("opening /dev/urandom: {e}"))?;
    let mut bytes = vec![0u8; n];
    f.read_exact(&mut bytes)
        .map_err(|e| format!("reading /dev/urandom: {e}"))?;
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        // Lowercase hex, two chars per byte.
        s.push(char::from_digit((b >> 4) as u32, 16).unwrap_or('0'));
        s.push(char::from_digit((b & 0x0f) as u32, 16).unwrap_or('0'));
    }
    Ok(s)
}

// ── Idempotent file writes ──────────────────────────────────────────────────
/// Write `contents` to `path` only if it does not already exist. Returns true if
/// written, false if it was left untouched (idempotency: never clobber).
fn write_if_absent(path: &Path, contents: &str) -> Result<bool> {
    if path.exists() {
        log(&format!("exists, leaving untouched: {}", path.display()));
        return Ok(false);
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("creating {}: {e}", parent.display()))?;
    }
    std::fs::write(path, contents)
        .map_err(|e| format!("writing {}: {e}", path.display()))?;
    ok(&format!("wrote {}", path.display()));
    Ok(true)
}

/// Like write_if_absent but sets 0600 (owner-only) — for plaintext secret files.
/// Unix-only, like the whole script (it also reads /dev/urandom).
fn write_secret_if_absent(path: &Path, contents: &str) -> Result<bool> {
    use std::os::unix::fs::OpenOptionsExt;
    if path.exists() {
        log(&format!("secret exists, leaving untouched: {}", path.display()));
        return Ok(false);
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("creating {}: {e}", parent.display()))?;
    }
    let mut f = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(path)
        .map_err(|e| format!("creating {}: {e}", path.display()))?;
    f.write_all(contents.as_bytes())
        .map_err(|e| format!("writing {}: {e}", path.display()))?;
    Ok(true)
}

// ── site.yaml starter (valid for the constructor's content schema) ──────────
/// The starter template. A raw string literal (not a `format!` with `\`-joins)
/// so YAML's significant indentation is preserved byte-for-byte. `__SLUG__` and
/// `__DOMAIN__` are substituted below. Section tags (`masthead`, `navStrip`,
/// `prose`) and the top-level keys (title/description/theme/nav/pages) map 1:1
/// to Swwstructor.Content's siteSpecFromJSON / pageSpecFromJSON / sectionSpecFromJSON.
const SITE_YAML_TEMPLATE: &str = r#"# swwstructor site spec for `__SLUG__` (__DOMAIN__)
# Edit content here — adding a section is a content edit, never a code edit.
title: "__SLUG__"
description: "A new swwstructor site."
# theme is optional; omit to use the default. A name (and colours) go here.
theme:
  name: "__SLUG__"
nav:
  - label: "Home"
    href: "/"
pages:
  - path: "/"
    title: "__SLUG__"
    sections:
      - section: masthead
        title: "__SLUG__"
        tagline: "made with swwstructor"
      - section: navStrip
        sticky: true
        links:
          - label: "Home"
            href: "/"
      - section: prose
        headline: "Welcome"
        body: >
          This is your new site. Replace this prose section with your own
          content. Each page is a list of sections; the layout engine places
          them responsively.
"#;

/// A minimal but schema-valid site: a masthead, a sticky nav, and one prose
/// section. Substitutes the slug/domain into the indentation-preserving template.
fn site_yaml(slug: &str, domain: &str) -> String {
    SITE_YAML_TEMPLATE
        .replace("__SLUG__", slug)
        .replace("__DOMAIN__", domain)
}

// ── age encryption ──────────────────────────────────────────────────────────
/// Resolve the --host-key argument to an age recipient string. If it names an
/// existing file, read the first non-empty line; otherwise treat the argument
/// itself as the recipient.
fn resolve_recipient(host_key: &str) -> Result<String> {
    let p = Path::new(host_key);
    if p.exists() {
        let contents = std::fs::read_to_string(p)
            .map_err(|e| format!("reading host key file {}: {e}", p.display()))?;
        let line = contents
            .lines()
            .map(str::trim)
            .find(|l| !l.is_empty())
            .ok_or_else(|| format!("host key file {} is empty", p.display()))?;
        Ok(line.to_string())
    } else {
        Ok(host_key.trim().to_string())
    }
}

/// Encrypt `secret` for `recipient` into `out_path` via `age -r <recipient> -o
/// <out_path>`, piping the secret to age's stdin so it never touches argv or a
/// temp file. Idempotent: skips if the .age already exists.
fn age_encrypt(recipient: &str, secret: &str, out_path: &Path) -> Result<bool> {
    if out_path.exists() {
        log(&format!(
            "encrypted secret exists, leaving untouched: {}",
            out_path.display()
        ));
        return Ok(false);
    }
    if let Some(parent) = out_path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("creating {}: {e}", parent.display()))?;
    }
    let out_str = out_path.to_string_lossy().into_owned();
    let mut child = Command::new("age")
        .args(["-r", recipient, "-o", &out_str])
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| format!("failed to spawn `age` (is it on PATH?): {e}"))?;
    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or("failed to open age stdin")?;
        stdin
            .write_all(secret.as_bytes())
            .map_err(|e| format!("writing secret to age stdin: {e}"))?;
        // stdin is dropped at the end of this block, sending EOF to age.
    }
    let status = child
        .wait()
        .map_err(|e| format!("waiting for age: {e}"))?;
    if !status.success() {
        return fail(format!(
            "age failed encrypting to {} (exit {:?})",
            out_path.display(),
            status.code()
        ));
    }
    // Print only the path, NEVER the secret.
    ok(&format!("wrote {}", out_path.display()));
    Ok(true)
}

// ── Paths ───────────────────────────────────────────────────────────────────
fn site_dir(slug: &str) -> PathBuf {
    PathBuf::from("sites").join(slug)
}
fn secrets_dir() -> PathBuf {
    PathBuf::from("deploy/secrets")
}
fn build_dir() -> PathBuf {
    PathBuf::from(".build/swwstructor-state")
}

// ── The Nix snippet to paste ────────────────────────────────────────────────
/// The instance attrset template. masterKeyFile / adminPasswordFile always point
/// at the .age files (Nix never references plaintext); whether those exist yet is
/// reported separately. Raw string preserves indentation for clean pasting.
const NIX_SNIPPET_TEMPLATE: &str = r#"  # Paste into deploy/flake.nix under services.swwstructor.instances:
  services.swwstructor.instances.__SLUG__ = {
    domain = "__DOMAIN__";
    port = __PORT__;
    siteDir = ../sites/__SLUG__;
    masterKeyFile = ./secrets/__SLUG__-master.age;
    adminPasswordFile = ./secrets/__SLUG__-admin.age;
    stage = "production";
  };
"#;

/// Render the `services.swwstructor.instances.<slug>` attrset for deploy/flake.nix.
fn nix_snippet(cfg: &Config) -> String {
    NIX_SNIPPET_TEMPLATE
        .replace("__SLUG__", &cfg.slug)
        .replace("__DOMAIN__", &cfg.domain)
        .replace("__PORT__", &cfg.port.to_string())
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
    log(&format!(
        "scaffolding site '{}' (domain={}, port={})",
        cfg.slug, cfg.domain, cfg.port
    ));

    // 1) Content starter (idempotent).
    let yaml_path = site_dir(&cfg.slug).join("site.yaml");
    write_if_absent(&yaml_path, &site_yaml(&cfg.slug, &cfg.domain))?;

    // 2) Generate secret material (32-byte hex master key + random admin pw).
    //    These live only in memory until written/encrypted; never logged.
    let master_key = random_hex(32)?;
    let admin_password = random_hex(18)?; // 36 hex chars — a strong random pw.

    // 3) Persist secrets: encrypted if a recipient was given, else plaintext+warn.
    let encrypted = match &cfg.host_key {
        Some(host_key) => {
            if !Command::new("age")
                .arg("--version")
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false)
            {
                return fail("`age` not found on PATH but --host-key was given (try: nix develop)");
            }
            let recipient = resolve_recipient(host_key)?;
            let master_out = secrets_dir().join(format!("{}-master.age", cfg.slug));
            let admin_out = secrets_dir().join(format!("{}-admin.age", cfg.slug));
            age_encrypt(&recipient, &master_key, &master_out)?;
            age_encrypt(&recipient, &admin_password, &admin_out)?;
            true
        }
        None => {
            // No recipient: write plaintext (0600) and warn loudly.
            let out = build_dir().join(format!("{}-secrets.txt", cfg.slug));
            let body = format!(
                "# swwstructor secrets for `{slug}` — PLAINTEXT, NOT for deploy.\n\
# age-encrypt these to deploy/secrets/{slug}-*.age before shipping.\n\
master_key={master}\n\
admin_password={admin}\n",
                slug = cfg.slug,
                master = master_key,
                admin = admin_password,
            );
            let wrote = write_secret_if_absent(&out, &body)?;
            if wrote {
                warn(&format!("wrote PLAINTEXT secrets to {}", out.display()));
                warn("these MUST be age-encrypted to deploy/secrets/ before deploy:");
                warn(&format!(
                    "  age -r <box-host-key> -o deploy/secrets/{}-master.age   (paste master_key)",
                    cfg.slug
                ));
                warn(&format!(
                    "  age -r <box-host-key> -o deploy/secrets/{}-admin.age    (paste admin_password)",
                    cfg.slug
                ));
                warn("or re-run with --host-key <recipient> to encrypt automatically.");
            } else {
                warn(&format!(
                    "{} already exists; left untouched (delete it to regenerate)",
                    out.display()
                ));
            }
            false
        }
    };

    // 4) Print the Nix snippet to paste (stdout — safe, no secrets in it).
    eprintln!();
    log("add this instance to deploy/flake.nix:");
    println!("{}", nix_snippet(&cfg));

    if encrypted {
        ok(&format!(
            "site '{}' scaffolded. Secrets encrypted to deploy/secrets/.",
            cfg.slug
        ));
    } else {
        ok(&format!(
            "site '{}' scaffolded. Encrypt its secrets before deploy (see warnings).",
            cfg.slug
        ));
    }
    Ok(())
}
