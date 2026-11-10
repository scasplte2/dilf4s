//! The JLVM call guest from the command line (`README.md`, "Proving a JLVM call"):
//!
//! ```text
//! dilf4s-zk-jlvm vkey
//! dilf4s-zk-jlvm execute (--case NAME | --call FILE)
//! dilf4s-zk-jlvm prove (--case NAME | --call FILE) --mode core|compressed|groth16 --out DIR [--cpu]
//! dilf4s-zk-jlvm vector [--proof NAME=FILE]... [--check]
//! ```
//!
//! A call is a row of `zk_jlvm_v1.json` by name (`--case`), or a JSON file `{"program": text, "data": text, "env":
//! {name: text}, "gasLimit": n}`, each text JLVM surface as `jlvm_v1.json` holds it; the host parses the texts and
//! writes the guest's stdin in canonical form (`Call::of_surface`). `prove` uses the CUDA prover when
//! built with the `cuda` feature, unless `--cpu`; `vector` rewrites `shared/vectors/zk_jlvm_v1.json`, taking each
//! `--proof` file (`proof-groth16.bin` from `prove`) for the call it names, or with `--check` only compares.

#![allow(clippy::print_stdout, clippy::print_stderr, reason = "a command-line tool reports by printing")]

use std::path::PathBuf;
use std::process::ExitCode;

use dilf4s_core::Value;
use dilf4s_zk_cases::call::{Call, decode_statement};
use dilf4s_zk_host::prove::{self, Backend, Mode};
use dilf4s_zk_host::{execute_call, guest_key, vector};
use sp1_sdk::{SP1Proof, SP1ProofWithPublicValues};

fn text(m: &dilf4s_core::value::Map, key: &str) -> Result<String, String> {
    m.get(key).and_then(Value::as_str).map(String::from).ok_or_else(|| format!("the call file has no text {key}"))
}

fn call_file(path: &str) -> Result<Call, String> {
    let body = std::fs::read_to_string(path).map_err(|e| format!("{path}: {e}"))?;
    let Value::Map(m) = dilf4s_core::json::parse(&body).map_err(|e| format!("{path}: {e}"))? else {
        return Err(format!("{path}: not a JSON object"));
    };
    let env = match m.get("env") {
        None => Vec::new(),
        Some(Value::Map(env)) => env
            .entries()
            .map(|(k, v)| v.as_str().map(|t| (k.to_string(), t.to_string())).ok_or_else(|| format!("binding {k}")))
            .collect::<Result<_, _>>()?,
        Some(_) => return Err(format!("{path}: env is not an object")),
    };
    let gas_limit = m
        .get("gasLimit")
        .and_then(Value::as_int)
        .and_then(|n| u64::try_from(n).ok())
        .ok_or_else(|| format!("{path}: no gasLimit"))?;
    Call::of_surface(&text(&m, "program")?, &text(&m, "data")?, &env, gas_limit)
        .map_err(|e| format!("{path}: not a call: {e:?}"))
}

/// The value following `flag`, if `flag` is present.
fn flag<'a>(args: &'a [String], flag: &str) -> Result<Option<&'a str>, String> {
    match args.iter().position(|a| a == flag) {
        None => Ok(None),
        Some(i) => {
            args.get(i.saturating_add(1)).map(|v| Some(v.as_str())).ok_or_else(|| format!("{flag} needs a value"))
        }
    }
}

fn the_call(args: &[String]) -> Result<Call, String> {
    match (flag(args, "--case")?, flag(args, "--call")?) {
        (Some(name), None) => vector::call(name),
        (None, Some(path)) => call_file(path),
        _ => Err(String::from("give exactly one of --case NAME and --call FILE")),
    }
}

async fn execute(args: &[String]) -> Result<(), String> {
    let call = the_call(args)?;
    let run = execute_call(&call).await?;
    println!("exit code {}, {} cycles, gas {:?}", run.exit_code, run.cycles(), run.report.gas());
    if run.exit_code == 0 {
        let s = decode_statement(&run.public_values)?;
        println!("statement {:?}", vector::statement_hex(&s));
        println!("public values {}", dilf4s_core::hex::encode(&run.public_values));
    }
    Ok(())
}

async fn prove(args: &[String]) -> Result<(), String> {
    let call = the_call(args)?;
    let mode = flag(args, "--mode")?.and_then(Mode::from_name).ok_or("--mode core|compressed|groth16")?;
    let out = PathBuf::from(flag(args, "--out")?.ok_or("--out DIR")?);
    let backend =
        if cfg!(feature = "cuda") && !args.iter().any(|a| a == "--cpu") { Backend::Cuda } else { Backend::Cpu };
    let executed = execute_call(&call).await?;
    if executed.exit_code != 0 {
        return Err(format!("the guest refuses the call (exit code {})", executed.exit_code));
    }
    println!("proving {} ({} cycles in execute mode) with the {backend:?} prover", mode.name(), executed.cycles());
    let proved = prove::prove(&call, mode, backend).await?;
    if proved.proof.public_values.as_slice() != executed.public_values.as_slice() {
        return Err(String::from("the proof's public values differ from the execution's"));
    }
    prove::write(&out, mode, &proved)?;
    println!("vkey {}, written to {}", proved.key.vkey, out.display());
    Ok(())
}

async fn write_vector(args: &[String]) -> Result<(), String> {
    let key = guest_key().await?;
    let mut proofs = Vec::new();
    for (i, a) in args.iter().enumerate() {
        if a != "--proof" {
            continue;
        }
        let spec = args.get(i.saturating_add(1)).ok_or("--proof NAME=FILE")?;
        let (name, file) = spec.split_once('=').ok_or("--proof NAME=FILE")?;
        let proof = SP1ProofWithPublicValues::load(file).map_err(|e| format!("{file}: {e:#}"))?;
        // bytes() panics on any other kind of proof, and an empty Groth16 proof is SP1's mock
        let real = matches!(&proof.proof, SP1Proof::Groth16(p) if !p.encoded_proof.is_empty());
        if !real {
            return Err(format!("{file}: not a real Groth16 proof"));
        }
        proofs.push((name.to_string(), proof.bytes()));
    }
    let committed = vector::committed()?;
    let expected = vector::expected(&key, committed.as_ref(), &proofs)?;
    let text = vector::render(&expected)?;
    let path = vector::path();
    if args.iter().any(|a| a == "--check") {
        let now = std::fs::read_to_string(&path).unwrap_or_default();
        return if now == text { Ok(()) } else { Err(format!("{} is stale", path.display())) };
    }
    std::fs::write(&path, text).map_err(|e| format!("{}: {e}", path.display()))?;
    let proved = expected.calls.iter().filter(|c| c.proof_bytes.is_some()).count();
    println!("{}: {} calls, {proved} proved, vkey {}", path.display(), expected.calls.len(), key.vkey);
    Ok(())
}

#[tokio::main]
async fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let rest = args.get(1..).unwrap_or_default();
    let done = match args.first().map(String::as_str) {
        Some("vkey") => guest_key().await.map(|k| println!("vkey {}\nelf sha256 {}", k.vkey, k.elf_sha256)),
        Some("execute") => execute(rest).await,
        Some("prove") => prove(rest).await,
        Some("vector") => write_vector(rest).await,
        _ => Err(String::from("usage: dilf4s-zk-jlvm vkey | execute | prove | vector (see the source's header)")),
    };
    match done {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}
