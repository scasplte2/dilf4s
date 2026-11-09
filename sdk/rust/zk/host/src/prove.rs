//! Proving a JLVM call with the guest `dilf4s-zk-jlvm-call` on a local prover: the CPU prover, or, with the `cuda`
//! feature, the CUDA prover. Never a network prover: the `network` feature of `sp1-sdk` is not enabled.
//!
//! In sp1-sdk v6.10.0 a proof's mode is chosen on the request (`ProveRequest::core`, `compressed`, `groth16` in
//! `src/prover/prove.rs`), and each request proves from the start: a Groth16 proof is the core proof, compressed,
//! shrunk, wrapped and then proved in gnark (`sp1-prover` `worker/prover/recursion.rs`, `run_groth16`), all in one
//! request. On the CUDA prover the whole request runs in `sp1-gpu-server` (`sp1-cuda` `client.rs`,
//! `Request::ProveWithMode`). On the CPU prover the gnark step runs in Docker, or natively with the `native-gnark`
//! feature (`sp1-recursion-gnark-ffi` `src/ffi/`).

use std::path::Path;

use sp1_sdk::{HashableKey as _, ProveRequest as _, Prover, ProverClient, ProvingKey as _, SP1ProofWithPublicValues};

use dilf4s_zk_cases::call::Call;

use crate::{GuestKey, JLVM_CALL, elf_sha256, stdin};

/// What to prove.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    /// The core (STARK) proof.
    Core,
    /// The core proof compressed to one recursive proof.
    Compressed,
    /// The compressed proof wrapped in Groth16 over BN254: what `groth16_verify` checks.
    Groth16,
}

impl Mode {
    /// The mode by its name: `core`, `compressed` or `groth16`.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "core" => Some(Self::Core),
            "compressed" => Some(Self::Compressed),
            "groth16" => Some(Self::Groth16),
            _ => None,
        }
    }

    /// The name.
    pub const fn name(self) -> &'static str {
        match self {
            Self::Core => "core",
            Self::Compressed => "compressed",
            Self::Groth16 => "groth16",
        }
    }
}

/// Which local prover.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Backend {
    /// The CPU prover.
    Cpu,
    /// The CUDA prover (`sp1-gpu-server`), with the `cuda` feature.
    Cuda,
}

/// A proof, the key it was made under, and SP1's own verification of it.
pub struct Proved {
    /// The proof with its public values.
    pub proof: SP1ProofWithPublicValues,
    /// The guest's key.
    pub key: GuestKey,
}

async fn run<P: Prover>(client: &P, call: &Call, mode: Mode) -> Result<Proved, String> {
    let pk = client.setup(JLVM_CALL).await.map_err(|e| format!("setup: {e}"))?;
    let request = client.prove(&pk, stdin(call));
    let request = match mode {
        Mode::Core => request.core(),
        Mode::Compressed => request.compressed(),
        Mode::Groth16 => request.groth16(),
    };
    let proof = request.await.map_err(|e| format!("prove ({}): {e}", mode.name()))?;
    client.verify(&proof, pk.verifying_key(), None).map_err(|e| format!("SP1 rejects its own proof: {e}"))?;
    Ok(Proved { proof, key: GuestKey { vkey: pk.verifying_key().bytes32(), elf_sha256: elf_sha256(&JLVM_CALL) } })
}

/// Proves `call` in `mode` on `backend`, and checks the proof with SP1's verifier.
pub async fn prove(call: &Call, mode: Mode, backend: Backend) -> Result<Proved, String> {
    match backend {
        Backend::Cpu => run(&ProverClient::builder().cpu().build().await, call, mode).await,
        #[cfg(feature = "cuda")]
        Backend::Cuda => run(&ProverClient::builder().cuda().build().await, call, mode).await,
        #[cfg(not(feature = "cuda"))]
        Backend::Cuda => Err(String::from("the CUDA prover needs the `cuda` feature")),
    }
}

/// Writes a proof's files into `dir`: `proof-<mode>.bin` (SP1's own encoding, `SP1ProofWithPublicValues::save`),
/// `public-values.hex`, `vkey.txt` (`programVKey`), `elf-sha256.txt`, and for Groth16 `proof-bytes.hex` (what
/// `groth16_verify` takes, `SP1ProofWithPublicValues::bytes`).
pub fn write(dir: &Path, mode: Mode, proved: &Proved) -> Result<(), String> {
    let io = |e: std::io::Error| format!("{}: {e}", dir.display());
    std::fs::create_dir_all(dir).map_err(io)?;
    proved.proof.save(dir.join(format!("proof-{}.bin", mode.name()))).map_err(|e| format!("save: {e:#}"))?;
    let public = dilf4s_core::hex::encode(proved.proof.public_values.as_slice());
    std::fs::write(dir.join("public-values.hex"), format!("{public}\n")).map_err(io)?;
    std::fs::write(dir.join("vkey.txt"), format!("{}\n", proved.key.vkey)).map_err(io)?;
    std::fs::write(dir.join("elf-sha256.txt"), format!("{}\n", proved.key.elf_sha256)).map_err(io)?;
    if mode == Mode::Groth16 {
        let bytes = dilf4s_core::hex::encode(&proved.proof.bytes());
        std::fs::write(dir.join("proof-bytes.hex"), format!("{bytes}\n")).map_err(io)?;
    }
    Ok(())
}
