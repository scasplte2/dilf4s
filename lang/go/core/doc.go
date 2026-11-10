// Package core is the dilf4s Go SDK's core: the canonical data model, its deterministic CBOR encoding and JSON
// mapping, tagged hashing, value commitments, the limits of shared/schemas/limits.cddl, and the runtime of the models
// generated from shared/schemas. The normative specification is docs/specs/canonical-encoding.md; the conformance
// vectors in shared/vectors are part of it.
//
// The error families core's own API returns (CanonicalError and StateKeyError from codec-errors.cddl, HashError from
// hash-errors.cddl) and the limits are generated into this package (the *_gen.go files): Go forbids the import cycle a
// separate package would make, as core returns them and every generated model package imports core. Every other
// schema is a package of its own under lang/scala/core/generated.
//
// Guarantees, for every exported function and method:
//
//   - no panic on any input, the zero value of every type included: a zero Value is null, and any other zero value is
//     refused where it cannot stand for anything (a zero DomainTag is InvalidDomainTag, a zero TaggedHash a
//     TypeMismatch);
//   - nothing is shared with the caller: a []byte or *big.Int passed in is copied before it is kept or read twice, and
//     one handed out is a fresh copy, so no caller can change a value after it is built;
//   - no walk of a value recurses on the native stack by the value's depth or size: CBOR, JSON, equality and the
//     commitments keep explicit stacks of at most MaxDepth open containers. Generated codecs recurse once per level of
//     the value or the model, and refuse to pass MaxDepth (DepthExceeded) before descending, as the Rust SDK's do;
//     and as a Go model may share its parts, encoding one charges every node it builds against a weight budget
//     (Encoding; DefaultEncodingWeight unless given), refusing as TooLarge rather than expanding the sharing;
//   - work is bounded by the input and the limits: a value's weight is cached, so a budgeted encoding or rendering of
//     a heavy value is refused in O(1), and numbers are refused by their bit length, never by rendering their digits;
//   - deterministic output: no map is ranged over where order matters, and a key a caller or a peer chooses is
//     compared, never hashed (a TextMap, and the duplicate checks of Map and the JSON parser);
//   - no goroutines, and no package-level variables: every value is immutable once built, so it is safe to share.
//
// Every error is one of the generated families' kinds (a value of a struct type implementing CanonicalError,
// StateKeyError or HashError), a DecodeFailure, a TooLarge or a DivisionByZero, checkable with errors.As.
//
// Regenerate the generated files with
// tools/sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain shared/schemas lang/ts/packages/core/src/generated lang/rust/core/src/generated lang/go".
package core
