//! Runs the dilf4s SP1 guests: the vector harness in execute mode (no proving), and the JLVM call guest
//! (`docs/specs/zk-jlvm.md`) in execute mode, for its verifying key, or proved ([`prove`]).

pub mod prove;
pub mod vector;

use dilf4s_core::hash::{Hasher as _, Sha256};
use dilf4s_zk_cases::call::Call;
use dilf4s_zk_cases::{Request, Response};
use sp1_sdk::{Elf, ExecutionReport, HashableKey as _, ProverClient, ProvingKey as _, SP1Stdin, include_elf};

/// The vector harness, built for `riscv64im-succinct-zkvm-elf` with the SP1 patches.
pub const GUEST: Elf = include_elf!("dilf4s-zk-guest");

/// The JLVM call guest: one call in, its statement's canonical CBOR committed as the public values.
pub const JLVM_CALL: Elf = include_elf!("dilf4s-zk-jlvm-call");

/// Executes the guest on `request`. A guest that panics or exits non-zero is an error.
///
/// The report has SP1's gas (its proving-cost estimate, precompile rows included). Gas is set explicitly: without it,
/// SP1 6.10.0's executor leaves the report's instruction and syscall counts at zero.
pub async fn execute(request: &Request) -> Result<(Response, ExecutionReport), String> {
    use sp1_sdk::Prover as _;
    let client = ProverClient::builder().light().build().await;
    let mut stdin = SP1Stdin::new();
    stdin.write(request);
    let (mut public_values, report) =
        client.execute(GUEST, stdin).calculate_gas(true).await.map_err(|e| format!("execute: {e:#}"))?;
    if report.exit_code != 0 {
        return Err(format!("the guest exited with {}", report.exit_code));
    }
    Ok((public_values.read::<Response>(), report))
}

/// The JLVM call guest's stdin for `call`.
pub fn stdin(call: &Call) -> SP1Stdin {
    let mut stdin = SP1Stdin::new();
    stdin.write(call);
    stdin
}

/// What one execution of the JLVM call guest gave: its exit code, its public values and SP1's report.
#[derive(Debug)]
pub struct Executed {
    /// The exit code: 0, or the guest's refusal (1: a call with no statement).
    pub exit_code: u64,
    /// The public values: the statement's canonical CBOR, or nothing.
    pub public_values: Vec<u8>,
    /// SP1's report.
    pub report: ExecutionReport,
}

impl Executed {
    /// The cycles: instructions plus syscalls.
    pub fn cycles(&self) -> u64 {
        self.report.total_instruction_count().saturating_add(self.report.total_syscall_count())
    }
}

/// Executes the JLVM call guest on `call` (execute mode: nothing is proved).
pub async fn execute_call(call: &Call) -> Result<Executed, String> {
    use sp1_sdk::Prover as _;
    let client = ProverClient::builder().light().build().await;
    let (public_values, report) =
        client.execute(JLVM_CALL, stdin(call)).calculate_gas(true).await.map_err(|e| format!("execute: {e:#}"))?;
    Ok(Executed { exit_code: report.exit_code, public_values: public_values.to_vec(), report })
}

/// The JLVM call guest's key: its SP1 verifying-key hash, as SP1's `bytes32` gives it (`0x` and 64 hex digits; the
/// `programVKey` of an SP1 Groth16 proof), and its ELF's SHA-256.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GuestKey {
    /// The verifying-key hash, `0x`-prefixed hex.
    pub vkey: String,
    /// The ELF's SHA-256, hex.
    pub elf_sha256: String,
}

impl GuestKey {
    /// The verifying-key hash's 32 bytes.
    pub fn vkey_bytes(&self) -> Result<Vec<u8>, String> {
        let digits = self.vkey.strip_prefix("0x").unwrap_or(&self.vkey);
        dilf4s_core::hex::decode(digits).map_err(|e| format!("vkey {}: {e}", self.vkey))
    }
}

/// The ELF's SHA-256, hex.
pub fn elf_sha256(elf: &Elf) -> String {
    dilf4s_core::hex::encode(&Sha256::hash_parts(&[elf.as_ref()]))
}

/// The JLVM call guest's key, from SP1's setup on the light prover: a preprocessing commitment, no proving.
pub async fn guest_key() -> Result<GuestKey, String> {
    use sp1_sdk::Prover as _;
    let client = ProverClient::builder().light().build().await;
    let pk = client.setup(JLVM_CALL).await.map_err(|e| format!("setup: {e:#}"))?;
    Ok(GuestKey { vkey: pk.verifying_key().bytes32(), elf_sha256: elf_sha256(&JLVM_CALL) })
}
