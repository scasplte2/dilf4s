//! The SP1 guest: reads a [`Request`], answers it with `dilf4s-zk-cases` and commits the [`Response`].

#![no_main]

use dilf4s_zk_cases::leaves::commit;
use dilf4s_zk_cases::{Body, Request, Response, run};

sp1_zkvm::entrypoint!(main);

/// The guest's entry point.
pub fn main() {
    let request: Request = sp1_zkvm::io::read();
    let body = match request {
        Request::Run(cases) => Body::Run(run(&cases)),
        Request::Leaves { form, leaves } => Body::Leaves(commit(form, &leaves)),
    };
    sp1_zkvm::io::commit(&Response { zkvm: cfg!(target_os = "zkvm"), body });
}
