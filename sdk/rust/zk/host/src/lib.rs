//! Runs the dilf4s SP1 guest in execute mode (no proving): the guest's ELF, built by `build.rs`, and a request in,
//! the guest's committed response and SP1's execution report out.

use dilf4s_zk_cases::{Request, Response};
use sp1_sdk::{Elf, ExecutionReport, ProverClient, SP1Stdin, include_elf};

/// The guest, built for `riscv64im-succinct-zkvm-elf` with the SP1 patches.
pub const GUEST: Elf = include_elf!("dilf4s-zk-guest");

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
