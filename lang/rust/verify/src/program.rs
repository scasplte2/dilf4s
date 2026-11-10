//! Reading JLVM programs (`dilf4s-jlvm`, docs/specs/jlvm.md section 1): the canonical-form decoder and the JSON
//! surface parser.

use dilf4s_jlvm::{Compiled, decode, parse, parse_value};

use crate::{Broken, holds};

/// `dilf4s_jlvm::decode(bytes)`. A program it decodes encodes back to `bytes`: a program has exactly one canonical form,
/// and the decoder accepts no other.
pub fn program_decode(bytes: &[u8]) -> Result<(), Broken> {
    let Ok(program) = decode(bytes) else { return Ok(()) };
    holds(program.encode().as_deref() == Ok(bytes), Broken::ProgramRoundTrip)
}

/// `dilf4s_jlvm::parse(text)`. A program it parses has a canonical form, which decodes back to the same program; and
/// compiling it gives a program or an error.
pub fn program_parse(text: &str) -> Result<(), Broken> {
    let Ok(program) = parse(text) else { return Ok(()) };
    // programs compared by their canonical encodings, their identity (see `canonical`)
    let bytes = program.encode().map_err(|_| Broken::SurfaceRoundTrip)?;
    holds(decode(&bytes).and_then(|p| p.encode()).as_ref() == Ok(&bytes), Broken::SurfaceRoundTrip)?;
    // the claim is only that compiling returns (Kani checks it does not panic); a program may still not compile
    let _compiled = Compiled::new(program);
    Ok(())
}

/// `dilf4s_jlvm::parse_value(text)`, input data or a host binding: it returns a value or an error. There is no claim
/// beyond that (Kani checks it does not panic): its JSON parse is [`crate::canonical::json_parse`]'s.
pub fn data_parse(text: &str) {
    let _value = parse_value(text);
}
