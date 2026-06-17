#!/usr/bin/env rust-script
//! Idempotent AWS EC2 NixOS provisioner for swwstructor.
//!
//! "Rust instead of bash": all the orchestration logic (typed AWS JSON,
//! find-or-create, state files, error handling) lives here; the heavy lifting
//! is delegated to the same battle-tested `aws` CLI via std::process::Command.
//!
//! Finds-or-creates ONE EC2 instance tagged `Name=<name>` from the official
//! NixOS x86_64 AMI, ensures an SSH key pair and a security group (22/80/443),
//! waits for it to be running, reads its IPs, and writes a state file at
//! `.build/swwstructor-state/<name>.json` that ship.rs reads for the target IP.
//!
//! Idempotent: re-running reuses any non-terminated instance tagged
//! `Name=<name>` rather than launching a second one, and refreshes the state
//! file. Safe to re-run.
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1", features = ["derive"] }
//! serde_json = "1"
//! ```
use serde::Deserialize;
use std::fmt::Write as _;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

// ── Result / error plumbing ────────────────────────────────────────────────
// A boxed error keeps the script dependency-light (no anyhow needed) while
// still letting `?` propagate from std::io, serde_json, and our own messages.
type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const LOG_PREFIX: &str = "[provision]";

fn log(msg: &str) {
    eprintln!("{LOG_PREFIX} {msg}");
}
fn ok(msg: &str) {
    eprintln!("{LOG_PREFIX} [ok] {msg}");
}

/// Make a plain error from a string (so `?` works on our own failures too).
fn fail<T>(msg: impl Into<String>) -> Result<T> {
    Err(msg.into().into())
}

// ── Defaults (the fixed contract with the CTO's Nix work) ───────────────────
const DEFAULT_NAME: &str = "swwstructor";
const DEFAULT_REGION: &str = "eu-west-2"; // London
const DEFAULT_TYPE: &str = "t3.small";
// Official NixOS x86_64-linux AMI for eu-west-2 (NixOS 25.05/26.05), same as onehr.
const DEFAULT_AMI: &str = "ami-009a6dc6bf97a4da7";
const DEFAULT_KEY_NAME: &str = "swwstructor-deploy";
const ROOT_VOL_GB: u32 = 30; // gp3 root; the AMI autoresizes its fs on boot.
const SG_NAME: &str = "swwstructor-sg";

// ── The single command helper ───────────────────────────────────────────────
/// Run `cmd args...`, capturing stdout. Errors (with stderr) on a non-zero exit
/// or if the binary cannot be spawned. This is the one place external I/O turns
/// into a `Result`, so callers only ever use `?` — never `.unwrap()`.
fn run(cmd: &str, args: &[&str]) -> Result<String> {
    let output = Command::new(cmd).args(args).output().map_err(|e| {
        format!("failed to spawn `{cmd}` (is it on PATH?): {e}")
    })?;
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

/// Run a command purely for its exit status (no captured output needed),
/// returning whether it succeeded. Used for "does this resource exist?" probes
/// where a non-zero exit is an expected, non-fatal "no".
fn run_ok(cmd: &str, args: &[&str]) -> bool {
    Command::new(cmd)
        .args(args)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// ── Typed AWS JSON (only the fields we need) ────────────────────────────────
#[derive(Deserialize)]
struct DescribeInstances {
    #[serde(rename = "Reservations", default)]
    reservations: Vec<Reservation>,
}

#[derive(Deserialize)]
struct Reservation {
    #[serde(rename = "Instances", default)]
    instances: Vec<Instance>,
}

#[derive(Deserialize)]
struct Instance {
    #[serde(rename = "InstanceId")]
    instance_id: String,
    #[serde(rename = "State")]
    state: InstanceState,
    #[serde(rename = "PublicIpAddress")]
    public_ip: Option<String>,
    #[serde(rename = "PrivateIpAddress")]
    private_ip: Option<String>,
}

#[derive(Deserialize)]
struct InstanceState {
    #[serde(rename = "Name")]
    name: String,
}

/// `aws ec2 run-instances` returns `{ "Instances": [ { "InstanceId": ... } ] }`.
#[derive(Deserialize)]
struct RunInstances {
    #[serde(rename = "Instances", default)]
    instances: Vec<MinInstance>,
}

#[derive(Deserialize)]
struct MinInstance {
    #[serde(rename = "InstanceId")]
    instance_id: String,
}

/// `aws ec2 create-security-group` returns `{ "GroupId": "sg-..." }`.
#[derive(Deserialize)]
struct CreateSg {
    #[serde(rename = "GroupId")]
    group_id: String,
}

/// `aws ec2 describe-security-groups` (we read the first group's id).
#[derive(Deserialize)]
struct DescribeSgs {
    #[serde(rename = "SecurityGroups", default)]
    security_groups: Vec<SgRef>,
}

#[derive(Deserialize)]
struct SgRef {
    #[serde(rename = "GroupId")]
    group_id: String,
}

// ── Config from args ────────────────────────────────────────────────────────
struct Config {
    name: String,
    region: String,
    instance_type: String,
    ami: String,
    key_name: String,
}

const USAGE: &str = "\
provision.rs — idempotent AWS EC2 NixOS provisioner for swwstructor

USAGE:
    rust-script automation/provision.rs [OPTIONS]

OPTIONS:
    --name <host>      instance Name tag / state-file key  (default: swwstructor)
    --region <region>  AWS region                          (default: eu-west-2)
    --type <type>      EC2 instance type                   (default: t3.small)
    --ami <ami-id>     NixOS x86_64 AMI                     (default: ami-009a6dc6bf97a4da7)
    --key-name <name>  EC2 key pair name                   (default: swwstructor-deploy)
    -h, --help         print this help and exit

The SSH public key is read from $SWW_SSH_PUBKEY or ~/.ssh/id_ed25519.pub and
imported as the key pair if it does not already exist.

Writes .build/swwstructor-state/<name>.json. Re-running reuses an existing
instance tagged Name=<name> (pending/running/stopped). Safe to re-run.";

/// Parse `--flag value` pairs. Unknown flags and missing values are hard errors
/// so a typo never silently provisions with a default.
fn parse_args(argv: &[String]) -> Result<Config> {
    let mut cfg = Config {
        name: DEFAULT_NAME.into(),
        region: DEFAULT_REGION.into(),
        instance_type: DEFAULT_TYPE.into(),
        ami: DEFAULT_AMI.into(),
        key_name: DEFAULT_KEY_NAME.into(),
    };
    let mut i = 0;
    while i < argv.len() {
        let arg = argv[i].as_str();
        // Each option consumes the following token; bail clearly if it's absent.
        let take = |i: &mut usize| -> Result<String> {
            *i += 1;
            argv.get(*i)
                .cloned()
                .ok_or_else(|| format!("option `{arg}` requires a value").into())
        };
        match arg {
            "--name" => cfg.name = take(&mut i)?,
            "--region" => cfg.region = take(&mut i)?,
            "--type" => cfg.instance_type = take(&mut i)?,
            "--ami" => cfg.ami = take(&mut i)?,
            "--key-name" => cfg.key_name = take(&mut i)?,
            "-h" | "--help" => {
                println!("{USAGE}");
                std::process::exit(0);
            }
            other => return fail(format!("unknown argument: {other}\n\n{USAGE}")),
        }
        i += 1;
    }
    Ok(cfg)
}

// ── Preflight ───────────────────────────────────────────────────────────────
/// Verify the tools and credentials we depend on before mutating anything in
/// AWS. A half-authenticated run leaves orphan resources (the onehr precedent),
/// so we abort early and loudly.
fn preflight(cfg: &Config) -> Result<()> {
    log("preflight: checking tooling and credentials");
    for tool in ["aws", "nix"] {
        if !run_ok(tool, &["--version"]) {
            return fail(format!(
                "required tool `{tool}` not found on PATH (try: nix develop)"
            ));
        }
    }
    // `sts get-caller-identity` is the canonical "are my creds live?" probe.
    let ident = run(
        "aws",
        &["--region", &cfg.region, "sts", "get-caller-identity", "--output", "text"],
    )
    .map_err(|e| {
        format!("aws credentials check failed (is the CLI configured?):\n{e}")
    })?;
    ok(&format!("aws identity: {}", ident.trim().replace('\t', " ")));
    Ok(())
}

// ── Key pair ────────────────────────────────────────────────────────────────
/// Resolve the operator's SSH public key path: $SWW_SSH_PUBKEY or the default.
fn pubkey_path() -> Result<PathBuf> {
    if let Ok(p) = std::env::var("SWW_SSH_PUBKEY") {
        return Ok(PathBuf::from(p));
    }
    let home = std::env::var("HOME").map_err(|_| "HOME is not set")?;
    Ok(PathBuf::from(home).join(".ssh/id_ed25519.pub"))
}

/// Ensure the EC2 key pair exists; import the operator's pubkey if absent.
/// Without this the official NixOS AMI authorizes no key and ship.rs can never
/// SSH in.
fn ensure_key_pair(cfg: &Config) -> Result<()> {
    if run_ok(
        "aws",
        &["--region", &cfg.region, "ec2", "describe-key-pairs", "--key-names", &cfg.key_name],
    ) {
        ok(&format!("key pair {} exists", cfg.key_name));
        return Ok(());
    }
    let pubkey = pubkey_path()?;
    if !pubkey.exists() {
        return fail(format!(
            "no SSH pubkey at {} (set $SWW_SSH_PUBKEY)",
            pubkey.display()
        ));
    }
    // `fileb://` makes the CLI read the file verbatim as the key material.
    let material = format!("fileb://{}", pubkey.display());
    run(
        "aws",
        &[
            "--region", &cfg.region, "ec2", "import-key-pair",
            "--key-name", &cfg.key_name,
            "--public-key-material", &material,
            "--tag-specifications",
            "ResourceType=key-pair,Tags=[{Key=Project,Value=swwstructor}]",
        ],
    )?;
    ok(&format!("imported key pair {} from {}", cfg.key_name, pubkey.display()));
    Ok(())
}

// ── Security group ──────────────────────────────────────────────────────────
/// Find-or-create the `swwstructor-sg` security group and ensure it allows
/// inbound TCP 22/80/443. Returns the group id. Ingress authorization is
/// idempotent: a duplicate rule returns `InvalidPermission.Duplicate`, which we
/// treat as success.
fn ensure_security_group(cfg: &Config) -> Result<String> {
    // Look it up by group-name first (idempotent reuse).
    let described = run(
        "aws",
        &[
            "--region", &cfg.region, "ec2", "describe-security-groups",
            "--filters", &format!("Name=group-name,Values={SG_NAME}"),
            "--output", "json",
        ],
    )?;
    let parsed: DescribeSgs = serde_json::from_str(&described)
        .map_err(|e| format!("parsing describe-security-groups JSON: {e}"))?;

    let sg_id = if let Some(sg) = parsed.security_groups.first() {
        ok(&format!("security group {SG_NAME} exists ({})", sg.group_id));
        sg.group_id.clone()
    } else {
        log(&format!("creating security group {SG_NAME}"));
        let created = run(
            "aws",
            &[
                "--region", &cfg.region, "ec2", "create-security-group",
                "--group-name", SG_NAME,
                "--description", "swwstructor: SSH + HTTP/S",
                "--output", "json",
            ],
        )?;
        let cs: CreateSg = serde_json::from_str(&created)
            .map_err(|e| format!("parsing create-security-group JSON: {e}"))?;
        ok(&format!("created security group {SG_NAME} ({})", cs.group_id));
        cs.group_id
    };

    // Authorize 22/80/443 (each separately so a partially-configured SG heals).
    for port in [22u16, 80, 443] {
        authorize_ingress(cfg, &sg_id, port)?;
    }
    Ok(sg_id)
}

/// Authorize one inbound TCP port from anywhere, tolerating a pre-existing rule.
fn authorize_ingress(cfg: &Config, sg_id: &str, port: u16) -> Result<()> {
    let port_s = port.to_string();
    let output = Command::new("aws")
        .args([
            "--region", &cfg.region, "ec2", "authorize-security-group-ingress",
            "--group-id", sg_id,
            "--protocol", "tcp",
            "--port", &port_s,
            "--cidr", "0.0.0.0/0",
        ])
        .output()
        .map_err(|e| format!("failed to spawn aws for ingress :{port}: {e}"))?;
    if output.status.success() {
        ok(&format!("authorized tcp/{port}"));
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    // A duplicate rule is the idempotent happy path, not a failure.
    if stderr.contains("InvalidPermission.Duplicate") {
        log(&format!("tcp/{port} already authorized"));
        Ok(())
    } else {
        fail(format!("authorizing tcp/{port} failed:\n{}", stderr.trim()))
    }
}

// ── Instance find-or-create ─────────────────────────────────────────────────
/// Find a non-terminated instance tagged `Name=<name>` (pending/running/stopped),
/// returning the first one if any. terminated/shutting-down are ignored so a
/// fresh run after a teardown re-creates cleanly.
fn find_instance(cfg: &Config) -> Result<Option<Instance>> {
    let out = run(
        "aws",
        &[
            "--region", &cfg.region, "ec2", "describe-instances",
            "--filters",
            &format!("Name=tag:Name,Values={}", cfg.name),
            "Name=instance-state-name,Values=pending,running,stopping,stopped",
            "--output", "json",
        ],
    )?;
    let parsed: DescribeInstances = serde_json::from_str(&out)
        .map_err(|e| format!("parsing describe-instances JSON: {e}"))?;
    Ok(parsed
        .reservations
        .into_iter()
        .flat_map(|r| r.instances)
        .next())
}

/// Launch a fresh instance from the AMI and return its instance id.
fn run_instance(cfg: &Config, sg_id: &str) -> Result<String> {
    log(&format!(
        "launching from {} ({}, {}GB gp3) in {}",
        cfg.ami, cfg.instance_type, ROOT_VOL_GB, cfg.region
    ));
    // Grow the root EBS volume; the official NixOS AMI's root device is
    // /dev/xvda and its filesystem autoresizes on boot.
    let bdm = format!(
        "DeviceName=/dev/xvda,Ebs={{VolumeSize={ROOT_VOL_GB},VolumeType=gp3,DeleteOnTermination=true}}"
    );
    let tags_instance = format!(
        "ResourceType=instance,Tags=[{{Key=Name,Value={}}},{{Key=Project,Value=swwstructor}}]",
        cfg.name
    );
    let tags_volume = format!(
        "ResourceType=volume,Tags=[{{Key=Name,Value={}-root}},{{Key=Project,Value=swwstructor}}]",
        cfg.name
    );
    let out = run(
        "aws",
        &[
            "--region", &cfg.region, "ec2", "run-instances",
            "--image-id", &cfg.ami,
            "--instance-type", &cfg.instance_type,
            "--security-group-ids", sg_id,
            "--key-name", &cfg.key_name,
            "--block-device-mappings", &bdm,
            // IMDSv2-only (no v1 fallback).
            "--metadata-options", "HttpTokens=required,HttpEndpoint=enabled",
            "--tag-specifications", &tags_instance, &tags_volume,
            "--count", "1",
            "--output", "json",
        ],
    )?;
    let parsed: RunInstances = serde_json::from_str(&out)
        .map_err(|e| format!("parsing run-instances JSON: {e}"))?;
    let id = parsed
        .instances
        .into_iter()
        .next()
        .map(|i| i.instance_id)
        .ok_or("run-instances returned no instance id")?;
    ok(&format!("launched {id}"));
    Ok(id)
}

/// Block until the instance reaches `running` (delegated to `aws ec2 wait`).
fn wait_running(cfg: &Config, instance_id: &str) -> Result<()> {
    log(&format!("waiting for {instance_id} to reach 'running'..."));
    run(
        "aws",
        &["--region", &cfg.region, "ec2", "wait", "instance-running", "--instance-ids", instance_id],
    )?;
    ok(&format!("{instance_id} is running"));
    Ok(())
}

/// Re-describe a single instance to read its (now-assigned) IPs.
fn describe_one(cfg: &Config, instance_id: &str) -> Result<Instance> {
    let out = run(
        "aws",
        &[
            "--region", &cfg.region, "ec2", "describe-instances",
            "--instance-ids", instance_id,
            "--output", "json",
        ],
    )?;
    let parsed: DescribeInstances = serde_json::from_str(&out)
        .map_err(|e| format!("parsing describe-instances JSON: {e}"))?;
    parsed
        .reservations
        .into_iter()
        .flat_map(|r| r.instances)
        .next()
        .ok_or_else(|| format!("instance {instance_id} vanished from describe-instances").into())
}

// ── State file ──────────────────────────────────────────────────────────────
fn state_dir() -> PathBuf {
    PathBuf::from(".build/swwstructor-state")
}

/// Seconds since the Unix epoch as an ISO-ish marker. std-only (no chrono): we
/// record the epoch seconds, which is unambiguous and good enough for state.
fn provisioned_at() -> String {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => format!("{}", d.as_secs()),
        Err(_) => "0".into(),
    }
}

/// Write `.build/swwstructor-state/<name>.json`. Hand-built so the file shape is
/// the exact contract ship.rs reads; serde_json::to_string would also work but
/// this keeps the field order obvious.
fn write_state(cfg: &Config, inst: &Instance) -> Result<PathBuf> {
    let dir = state_dir();
    std::fs::create_dir_all(&dir)
        .map_err(|e| format!("creating {}: {e}", dir.display()))?;
    let path = dir.join(format!("{}.json", cfg.name));
    let public_ip = inst.public_ip.clone().unwrap_or_default();
    let private_ip = inst.private_ip.clone().unwrap_or_default();

    // serde_json::json! would pull no extra dep (it's in serde_json) and gives
    // correct escaping for free.
    let value = serde_json::json!({
        "name": cfg.name,
        "instanceId": inst.instance_id,
        "publicIp": public_ip,
        "privateIp": private_ip,
        "region": cfg.region,
        "instanceType": cfg.instance_type,
        "ami": cfg.ami,
        "provisionedAt": provisioned_at(),
    });
    let pretty = serde_json::to_string_pretty(&value)
        .map_err(|e| format!("serializing state: {e}"))?;
    std::fs::write(&path, format!("{pretty}\n"))
        .map_err(|e| format!("writing {}: {e}", path.display()))?;
    Ok(path)
}

// ── main ────────────────────────────────────────────────────────────────────
fn main() {
    // Skip argv[0] (the script path). Keep main thin: real work in `provision`,
    // with one place to print errors and set the exit code.
    let argv: Vec<String> = std::env::args().skip(1).collect();
    if let Err(e) = real_main(&argv) {
        eprintln!("{LOG_PREFIX} ERROR: {e}");
        std::process::exit(1);
    }
}

fn real_main(argv: &[String]) -> Result<()> {
    let cfg = parse_args(argv)?;
    log(&format!("provisioning '{}' in {}", cfg.name, cfg.region));

    preflight(&cfg)?;
    ensure_key_pair(&cfg)?;
    let sg_id = ensure_security_group(&cfg)?;

    // Find-or-create, then make sure it's running and re-read for IPs.
    let instance_id = match find_instance(&cfg)? {
        Some(found) => {
            ok(&format!(
                "reusing existing instance {} (state: {})",
                found.instance_id, found.state.name
            ));
            // A stopped instance must be started before ship.rs can reach it.
            if found.state.name == "stopped" {
                log("instance is stopped; starting it...");
                run(
                    "aws",
                    &[
                        "--region", &cfg.region, "ec2", "start-instances",
                        "--instance-ids", &found.instance_id,
                    ],
                )?;
            }
            found.instance_id
        }
        None => {
            log("no existing instance found; creating one");
            run_instance(&cfg, &sg_id)?
        }
    };

    wait_running(&cfg, &instance_id)?;
    let inst = describe_one(&cfg, &instance_id)?;

    if inst.public_ip.as_deref().unwrap_or("").is_empty() {
        log("warning: instance has no public IP yet (re-run in a moment if it persists)");
    }

    let path = write_state(&cfg, &inst)?;
    ok(&format!("wrote {}", path.display()));

    // Human-readable summary on stderr; keep stdout clean for the next step.
    let mut summary = String::new();
    let _ = writeln!(summary, "  instanceId : {}", inst.instance_id);
    let _ = writeln!(
        summary,
        "  publicIp   : {}",
        inst.public_ip.as_deref().unwrap_or("<none>")
    );
    let _ = writeln!(
        summary,
        "  privateIp  : {}",
        inst.private_ip.as_deref().unwrap_or("<none>")
    );
    eprint!("{summary}");

    println!(
        "{LOG_PREFIX} done. next: ./automation/ship.rs --name {} boot",
        cfg.name
    );
    Ok(())
}
