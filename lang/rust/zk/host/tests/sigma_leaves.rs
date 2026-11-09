//! Issue #33 in the guest: each sigma leaf commitment as two scalar multiplications and as one k256 linear
//! combination, under SP1's patched k256. Both forms must give the native commitments. A form's cost is its run's
//! less a baseline run's (same decoding, encoding and I/O, no multiplication), in cycles (instructions plus syscalls)
//! and in SP1 gas (its proving-cost estimate, precompile rows included); printed with
//! `cargo test --release --test sigma_leaves -- --nocapture`. The native times are the `sigma_leaves` example of
//! `dilf4s-zk-cases`.

#![allow(
    clippy::unwrap_used,
    clippy::panic,
    clippy::print_stdout,
    clippy::as_conversions,
    clippy::cast_precision_loss,
    reason = "tests fail by panicking and report by printing"
)]

use dilf4s_zk_cases::leaves::{Form, Leaf, commit};
use dilf4s_zk_cases::vectors::leaves;
use dilf4s_zk_cases::{Body, Request};
use dilf4s_zk_host::execute;

/// A run's cost: cycles and gas.
#[derive(Clone, Copy)]
struct Cost {
    cycles: u64,
    gas: u64,
}

/// The guest's commitments for `form` and what its run cost.
async fn guest(form: Option<Form>, leaves: &[Leaf]) -> (Vec<Vec<u8>>, Cost) {
    let (response, report) = execute(&Request::Leaves { form, leaves: leaves.to_vec() }).await.unwrap();
    assert!(response.zkvm);
    let Body::Leaves(commitments) = response.body else { panic!("the guest answered another request") };
    let cycles = report.total_instruction_count().saturating_add(report.total_syscall_count());
    (commitments.unwrap(), Cost { cycles, gas: report.gas().unwrap() })
}

#[tokio::test]
async fn leaf_commitments_in_both_forms() {
    let (generator, other) = leaves().unwrap();
    for (label, group) in [("generator (dlog)", generator), ("other (DH tuple)", other)] {
        assert!(!group.is_empty(), "{label}: no leaves");
        let native = commit(Some(Form::TwoMuls), &group).unwrap();
        assert_eq!(commit(Some(Form::Lincomb), &group).unwrap(), native, "{label}: natively, the forms agree");

        let (_, base) = guest(None, &group).await;
        let (two_muls, a) = guest(Some(Form::TwoMuls), &group).await;
        let (lincomb, b) = guest(Some(Form::Lincomb), &group).await;
        assert_eq!(two_muls, native, "{label}: the guest's two multiplications give the native commitments");
        assert_eq!(lincomb, native, "{label}: the guest's linear combination gives the native commitments");

        let n = group.len() as f64;
        let per = |c: u64, base: u64| c.saturating_sub(base) as f64 / n;
        let (ac, bc) = (per(a.cycles, base.cycles), per(b.cycles, base.cycles));
        let (ag, bg) = (per(a.gas, base.gas), per(b.gas, base.gas));
        println!(
            "guest {label}: {} leaves; per leaf, two muls {ac:.0} cycles / {ag:.0} gas, lincomb {bc:.0} cycles / \
             {bg:.0} gas; lincomb/two muls {:.3} cycles, {:.3} gas",
            group.len(),
            bc / ac,
            bg / ag
        );
    }
}
