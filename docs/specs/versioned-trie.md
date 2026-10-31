# dilf4s versioned trie (v1)

Status: normative. Conformance vectors: `shared/vectors/versioned_trie_v1.json`. Code:
`xyz.kd5ujc.accumulators.versioned`.

A versioned trie is a key-value state that an application changes by committing a batch of writes at a version.
Every commit yields **one root**, which commits to three things: the version, the state at it, and the state root of
every version committed so far. A single trusted root therefore answers questions about the current state, about
whether a version was committed, and about the state at any past version, with the trie proofs of `trie.md`.

## 1. Model

- The **state** is a map from state keys (§2) to canonical values (`canonical-encoding.md` §1).
- A **commit** applies a batch of writes (puts and deletes, the last write to a key winning) at a version `V`.
  Versions are strictly increasing; gaps are allowed.
- The **head** is the latest commit: `(V, stateRoot, historyRoot)`.
- The **state trie** is a binary Patricia trie (`trie.md`) in mode `variable(256)` over `utf8(key) → commit(value)`.
- The **history trie** is a binary Patricia trie in mode `fixed(w)` over `vkey(u) → commit(stateRoot_u)` for every
  committed version `u ≤ V`. It includes `V` itself.
- The **root** is the root of the top trie (§3).

`commit(v)` is the value commitment of `trie.md` §5: `0x00 ‖ b` for a byte string `b`, `0x01 ‖ cbor(v)` otherwise. A
trie commits `valueHash(commit(v))`, so a JLVM program claims a value as itself, with a plain trie mode.

## 2. Keys and versions

**State keys** follow the grammar `segment *("/" segment)`, where a segment matches `[a-z0-9][a-z0-9._-]*`. A segment
has at most 64 characters, a key at most 16 segments and 256 characters. A key's trie key is its UTF-8 bytes; since the
alphabet is ASCII, byte order is string order.

A **namespace** `ns` is a key naming the keys below `ns/`. Its prefix is `utf8(ns + "/")`, so namespace `fiber` never
matches `fiberx/…`. A key under a namespace has at least one more segment, so a namespace has at most 254 characters
and 15 segments. Namespaces carry no meaning to the trie: none is reserved.

Keys are not hashed (`trie.md` §1), which is what makes namespace and range proofs possible. An application chooses
its keys, so a key layout can deepen a path: proof size and gas grow with the bits shared along it, up to 2048 bits.
Applications that accept keys from untrusted parties should bound or hash those keys themselves.

**Version keys.** A version type has a version key `vkey`: a fixed width `w` and an unsigned big-endian encoding that
preserves order. Comparing two versions and comparing their keys as unsigned bytes always agree. The default is a
non-negative signed 64-bit integer, `0 ≤ v ≤ 2^63 − 1`, as 8 bytes. For those versions it is exactly the JLVM's
`int_to_bytes [v, 8]`. It is **not** the storage key codec for integers (`storage.md` §1), which flips the sign bit and
so would give a program a different key. `w` is configuration, part of what a verifier trusts along with the root.

The cap is part of the key. A store refuses to commit a version above `2^63 − 1`, and a native verifier refuses to
encode one, or to decode a key whose top bit is set, as `InvalidVersion`. The JLVM has no version type:
`int_to_bytes [v, 8]` encodes every `v` below `2^64` (and is a `Domain` error from `2^64`). A key it builds for
`2^63 ≤ v < 2^64` is therefore never in a history trie, so a program's proof about such a version shows it absent, or
does not cover it. It is never a committed version. The `versionKeys` vectors pin both sides of the cap.

## 3. The root

The root is the root of a trie in mode `fixed(1)`, the **top trie**, with three entries:

| Slot | Value committed |
|---|---|
| `0x00` | `commit(vkey(V))` |
| `0x01` | `commit(stateRoot)` |
| `0x02` | `commit(historyRoot)` |

Each value is a byte string, so each slot holds `valueHash(0x00 ‖ x)`. Slots `0x03..0xff` are reserved for extensions.
Every proof carries the whole top trie (five records), so an unused slot is provably absent and a verifier checks only
the slots it uses. The top trie is recomputed from the head; it is never stored.

**Head invariant.** `history[vkey(V)] = commit(stateRoot)`, and `vkey(V)` is the history trie's largest key. Slot
`0x01` duplicates the history entry for `V`, so that a current-state proof needs no history proof. A store
establishes the invariant on every commit; a replica (§7) and the native head verifier (§6) check it. A JLVM program
trusts the producer for it, as it trusts the root.

**Epochs.** With big-endian keys, an epoch of `2^k` consecutive versions is a range of history keys. For a byte-aligned
epoch (`2^8`, `2^16`, …) it is a key prefix. A prefix or range proof lists an epoch completely, and once every version
in an epoch is past, the subtree under its prefix never changes. No separate roll-up is needed.

## 4. Commits

A commit takes the version the caller expects to be the head (none before the first commit), the next version and a
batch:

1. A null value deletes its key, as a null map value is absent (`canonical-encoding.md` §1). A null value could not
   be proven present anyway: in `trie_verify`, a null claims absence. A value is at most 1 MiB (1048576 bytes) of
   canonical CBOR, so that every entry fits one snapshot chunk (§7), and at most 253 levels deep (a scalar is 0). The
   records that carry values, a delta's upserts and a snapshot's state chunks, wrap each in a map, an array and a map,
   and every record stays within the canonical depth limit of 256 (`canonical-encoding.md`). A larger or deeper value is
   refused at commit, so a store can always serve every delta and snapshot of what it accepted.
2. **Recommit.** If the next version is the head's and the expected version is the head's parent, the commit is a
   retry: it succeeds without writing if the batch, applied to the parent's state, gives the head's state root, and is
   refused as a rewrite otherwise. Only the head can be recommitted.
3. Otherwise the expected version must be the head's (a version conflict if not), and the next version must be greater
   (not increasing if not).
4. The state trie, the history trie, the value store and the version index move together, in **one** atomic
   compare-and-write (`storage.md` §3). A crash leaves all of them at the previous head or all at the new one.

**Previews.** The same computation without the write is a **draft**: the head and root a commit would make. A draft
can be extended by further drafts, each reading through the ones below it; committing a draft writes its whole chain
at once, or nothing if the store moved since it was staged. A producer that must announce a root before adopting a
state (a consensus round, for example) previews it, and commits the preview it settles on.

**Rollback** to a retained version discards every later version from every store and the index, in one write.

**Retention.** Pruning through a version `p` discards past states and values before `p`. Afterwards:
- reading a key at a version before `p`, or proving the state there, is "not retained";
- the delta into `p` or any earlier version is "not retained" (§7);
- the history trie stays whole at the head. Every version's inclusion and absence stays provable, and every proof
  made earlier still verifies against its root.

Retention is node-local policy: it never changes a root.

**Restore.** A store can start from a snapshot instead of from its first commit. Restore takes a head the caller
already trusts and a snapshot, checks the snapshot against it (§7), and writes it into an **empty** store in one
compare-and-write; a store that is not empty is refused. The restored store is then exactly the store that committed
every version of the snapshot's history and pruned through its head `V_b`, byte for byte except each part's
generation (§10), which counts the mutations that got it there:
- every version, before `V_b` included, stays attestable: inclusion, absence, as-of and version listings;
- the state and values of versions before `V_b` are "not retained", as below a prune floor; so is the delta into `V_b`;
- rollback below `V_b` is "not retained"; commits continue from `V_b`.

Nothing is fabricated. The stores below the index hold only `V_b`, as after a prune. The index holds every version with
the state root the history lists, and those are the roots the trusted history root commits to (§10).

## 5. Proofs

A proof is the head, the whole top trie, and optionally a history part and a state part:

- `at` names a version `u` asked about and, if it was committed, its state root. It is omitted for a proof about the
  current state, for a head proof and for an as-of answer of "none".
- `history` is a proof over the head's history root: an inclusion or absence of `vkey(u)`, a range, or the tail from
  the head version up.
- `state` is a proof over the state root of `at` if present, otherwise of the head: keys, namespaces (prefixes) and
  ranges, as in `trie.md` §4.

Trie proofs carry no keys: the verifier supplies every key, prefix and range. A proof therefore cannot redirect a query
to another key, version or epoch width.

The normative shape is `shared/schemas/versioned-trie.cddl`. It is reproduced here verbatim and checked by a test;
generated wire types live in `xyz.kd5ujc.models.versioned_trie`:

```cddl
; dilf4s versioned trie, v1 (docs/specs/versioned-trie.md). Normative for shape.
; Trie proofs are trie.cddl's; they carry no keys, so the verifier supplies every key, prefix and range. A version
; key "v" is the version's fixed-width unsigned big-endian encoding; roots are digests. Domain invariants (the width
; of "v", digest sizes, the key grammar's length limits) are checked by the domain decoder, not by these types.
;# import trie as trie

versioned-trie-head = {
  "v": bytes,
  "s": bytes,
  "h": bytes,
}

versioned-trie-at = {
  "v": bytes,
  ? "s": bytes,
}

versioned-trie-proof = {
  "head": versioned-trie-head,
  "top": trie.trie-proof,
  ? "at": versioned-trie-at,
  ? "history": trie.trie-proof,
  ? "state": trie.trie-proof,
}

state-key = text .regexp "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*"

; A value is at most 253 levels deep, so that this record inside a delta or a snapshot chunk (a map, an array and a
; map around it) stays within the canonical depth limit of 256.
versioned-trie-upsert = {
  "k": state-key,
  "v": any,
}

versioned-trie-delta = {
  ? "parent": versioned-trie-head,
  "head": versioned-trie-head,
  "upserts": [* versioned-trie-upsert],
  "removes": [* state-key],
}

; A snapshot is a stream of chunks, each one canonical CBOR record of at most 4194304 bytes: one head chunk, then state
; chunks whose keys strictly increase across all of them, then history chunks whose versions strictly increase across
; all of them and end at the head version, with the head's state root. Values are at most 1048576 bytes of canonical
; CBOR, at most 253 levels deep, and never null. A receiver checks each chunk as it arrives, and the roots once the
; history reaches the head.
versioned-trie-snapshot-chunk = versioned-trie-snapshot-head / versioned-trie-snapshot-state / versioned-trie-snapshot-history

versioned-trie-snapshot-head = { "head": versioned-trie-head }

versioned-trie-snapshot-state = { "state": [1*1024 versioned-trie-upsert] }

versioned-trie-snapshot-history = { "history": [1*1024 versioned-trie-version] }

; A committed version (its version key) and its state root.
versioned-trie-version = {
  "v": bytes,
  "s": bytes,
}
```

Values travel separately, as everywhere in `trie.md` §5. A delta's values are canonical values; its keys are state
keys as text.

## 6. Verification

Every check is against a trusted root `R` and the configured width `w`. "Not covered" (a proof that reaches a stub
where the query needs an answer) is an error, never a negative answer.

1. **Binding.** The head's top trie must hash to `R`, and the proof's top trie must verify against `R` with slots
   `0x00`, `0x01` and `0x02` present and holding the head's values.
2. **Current state.** No `at`; the state part verifies against `stateRoot`. A key is present with `valueHash(commit(x))`,
   or absent. A namespace or range listing is complete.
3. **Version `u`.** `at.v = vkey(u)`. The history part verifies against `historyRoot`, and `vkey(u)` is either present
   with `commit(at.s)` or absent with no `at.s`.
4. **A key at version `u`.** Rule 3 with `u` present, then the state part verifies against `at.s`.
5. **As of `t`.** If `at` names `p`, then `vkey(p) ≤ vkey(t)` and the history range `[vkey(p), vkey(t) ‖ 0x00)` lists
   exactly `(vkey(p), commit(at.s))`. If there is no `at`, the range `[empty, vkey(t) ‖ 0x00)` lists nothing. The upper
   bound sorts after `vkey(t)` and before every other `w`-byte key above it, because a proper prefix sorts first and
   range bounds need not be keys (`trie.md` §4).
6. **Versions in `[a, b)`.** The history range lists exactly the claimed versions with their state roots.
7. **Head.** Rule 1, and the history range `[vkey(V), ∞)` lists exactly `(vkey(V), commit(stateRoot))`: the head
   invariant.

## 7. Replication

Transport is out of scope; these are the data and the checks.

- **Delta.** The change a version made: the parent head (none for the first commit), the head, the upserts and the
  removes. A store serves the delta of every retained version; below the prune floor it is "not retained", and a
  follower falls back to a snapshot.
- **Snapshot.** The head, every current entry in key order, then every committed version with its state root in
  version order, as a stream of chunks (below). A store reads it in pieces; if a commit lands while it is read, it is
  refused rather than torn.
- **Checking a snapshot.** A receiver checks it against a head it **already trusts**, never against the snapshot's
  own claim, chunk by chunk as it arrives, and refuses it at the first chunk that breaks a rule:
  1. the first chunk is the head, and it is the trusted head; no other head chunk follows;
  2. state chunks come next; their keys strictly increase across all of them (so none repeats), and no value is null,
     over 1 MiB or over 253 levels deep;
  3. history chunks come last; their versions strictly increase across all of them, none is after the trusted
     version, and the trusted version, when it comes, holds the trusted state root and ends the snapshot;
  4. every state or history chunk holds 1 to 1024 items.

  Once the history reaches the trusted version, the entries must rebuild the trusted state root, and the versions
  the trusted history root. A snapshot that ends earlier is truncated.
- **Replica.** A replica starts empty, or from a snapshot checked as above. A replica
  applies a delta only if the delta's parent is its head, its version is greater, no key is both written and removed,
  and the head it recomputes (state root, then history root with the new version) is the delta's head.

**Snapshot wire format.** Each chunk is one canonical CBOR record (`versioned-trie-snapshot-chunk`, §5) of at most
4 MiB (4194304 bytes); a receiver refuses a larger chunk before decoding it. A head chunk is `{"head": head}`, a state
chunk `{"state": [{"k": key, "v": value}, …]}`, a history chunk `{"history": [{"v": vkey, "s": stateRoot}, …]}`. A
producer closes a chunk before it would pass 1024 items or the byte limit; one entry always fits, since a value is at
most 1 MiB and a key 256 bytes. How chunks are framed and carried is the transport's concern.

## 8. Projection (informative)

The core takes batches; it never diffs states. An application whose state is a value `S` can supply a view: the
entries of `S`, and a delta between two states, which by default is the structural difference of their entries. A
hand-written delta is faster but can disagree with the entries; the opt-in check `Projection.verifyAgainst` rebuilds
the state root from the entries and compares it with the committed one.

## 9. Verifying in the JLVM

No operation is specific to versioned tries. A program uses `trie_verify`, `trie_prefix_verify`, `trie_range_verify`,
`int_to_bytes` and `cat` (`jlvm.md` §6), in plain trie modes. The trusted inputs are the root `R` and the width
`w`; intermediate roots pass as byte witnesses, and each is bound by the check before it. Modes are maps, written
`{"object": {"fixed": 1}}`; the table abbreviates them.

| Claim | Program |
|---|---|
| Slot binding | `trie_verify [{fixed:1}, R, 0x00, vkey(V), top]`; likewise `0x01 → s` and `0x02 → h`. Check only the slots used |
| Current key `k = x` (absent: `x = null`) | slot `0x01` check `and trie_verify [{variable:256}, s, utf8(k), x, state]` |
| Namespace slice | slot `0x01` check `and trie_prefix_verify [{variable:256}, s, utf8(ns + "/"), entries, state]` |
| Version `u` committed with `s_u` / never committed | slot `0x02` check `and trie_verify [{fixed:w}, h, int_to_bytes [u, w], s_u or null, history]` |
| Key at version `u` | the previous row `and trie_verify [{variable:256}, s_u, utf8(k), x, state]` |
| As of `t` (latest `p ≤ t`) | slot `0x02` check `and trie_range_verify [{fixed:w}, h, int_to_bytes [p, w], cat(int_to_bytes [t, w], 0x00), [[int_to_bytes [p, w], s_p]], history]` |
| No version at or before `t` | slot `0x02` check `and trie_range_verify [{fixed:w}, h, 0x, cat(int_to_bytes [t, w], 0x00), [], history]` |
| Epoch listing | slot `0x02` check `and trie_prefix_verify [{fixed:w}, h, epoch prefix, entries, history]` |

- **Keys come from the program.** Build history keys with `int_to_bytes [u, w]`, never from the proof. There is no
  text-to-UTF-8 operation, so a state key is a byte literal in the program (`"0x…"`) or a bytes input.
- **Not covered is an error.** A program that wants a boolean wraps the check.
- **Check slots one at a time.** A complete listing of the top trie (`trie_range_verify [{fixed:1}, R, 0x, null,
  [all slots], top]`) is an opt-in "exact head" check; programs that use it break when an extension slot is added.
- **Epoch prefixes need byte-aligned epochs** (`2^8`, `2^16`, …), because a prefix is bytes. Other epochs use
  `trie_range_verify`.

A worked example, a key `k` with value `x` at version `u`, with the witnesses as data:

```json
{"and": [
  {"trie_verify": [{"object": {"fixed": 1}}, {"var": "root"}, "0x02", {"var": "h"}, {"var": "top"}]},
  {"trie_verify": [{"object": {"fixed": 8}}, {"var": "h"}, {"int_to_bytes": [{"var": "u"}, 8]}, {"var": "su"}, {"var": "history"}]},
  {"trie_verify": [{"object": {"variable": 256}}, {"var": "su"}, {"var": "k"}, {"var": "x"}, {"var": "state"}]}
]}
```

## 10. Storage (informative)

One raw store holds every part, each under its own namespace (`storage.md` §1):

| Namespace | Contents |
|---|---|
| `<n>/state/*` | the state trie's nodes and per-version roots (`trie.md` §6) |
| `<n>/hist/*` | the history trie's nodes and per-version roots |
| `<n>/vals/*` | the values: current, per-version log, per-key history (`storage.md` §2) |
| `<n>/vidx/v` | `vkey(V) → seq` |
| `<n>/vidx/s` | `seq → vkey(V) ‖ stateRoot` |

Each part keeps a dense internal sequence, `0` before the first commit, and every write moves all of them, so their
sequences are equal; opening a store checks this. The index maps between versions and sequences, and also records each
version's state root, so version attestations, as-of answers and snapshots survive pruning of old states. Pruning never
touches the index.

**Revisions.** The four parts are written together, but read one record at a time, and other writers may share the
backend. Each part's guarded records hold a generation that every mutation advances and that never repeats
(`storage.md` §3), so the parts' records together form a **revision** that no later state of the store can match:
- **Mutations** (commit, preview and its drafts, rollback, prune, restore) read the revision **before anything else**,
  decide and stage on what they read after it, and guard their one compare-and-write on it alone. If it lands, nothing
  they read had changed; if another writer moved the store in between, it is refused as a version conflict and can be
  retried. A draft keeps the revision it was staged against, and so does every draft built on it: a draft from before a
  rollback is refused even after the store re-reaches its sequence, which a guard on sequences alone would accept.
- **Reads** that combine several records (the head, a past head or value, every proof, a delta, a snapshot, the
  recommit check, and the alignment check on opening) read the revision before and after, and refuse the answer if
  it changed, rather than mix two states.
- A read that fails inside either kind, because a record it followed was removed or replaced meanwhile, is judged
  by the revision too: if the store moved, the failure is a version conflict, not corruption; if it did not, the
  failure stands.
- Single reads (the current value of a key, a scan of current entries) need no revision.

**Sequences after a restore.** The `i`-th version of a restored history (from 1) gets sequence `i`, the sequence it
had in the store that committed it, so the head `V_b` gets the length of the history. The index holds an entry for
every one of them. The tries and the values store hold only sequence `V_b`, with their floor there, which is exactly
how a store pruned through `V_b` holds them: sequences below a floor have no state, no root and no log. A sequence
that starts above zero, or an index keyed by version alone, was not needed: the dense sequence already describes a
pruned store, and a restore is a pruned store that never held the pruned states.

## 11. Deliberate changes from metakit

metakit's committed state (`lifecycle/committed`) is the predecessor. Its tessellation wiring (DataApplication hooks,
the on-chain breadcrumb, HTTP routes, `SnapshotOrdinal`, the work cache that served tessellation's call orderings) is not
part of this library.

| metakit | dilf4s | Why |
|---|---|---|
| Two roots, `sha256(raw(mptRoot) ‖ raw(catalogRoot))`, untagged, not binding the ordinal | One root: the top trie of §3, tagged through the trie tags, binding the version | Domain separation; the version is attested; slots are extensible and absence is provable |
| History lags by one: the catalog at `N` holds ordinals `≤ N−1`, so the head ordinal attests "not committed" | The history at `V` includes `V` | The lag was a tessellation call-ordering artifact |
| Epoch roll-up: hot, level-1 and sealed SMTs; `epochSize` consensus-critical | One `fixed(w)` history trie; an epoch is a key prefix or range | Proofs are one inclusion at any age; nothing to configure but `w`; no seal step to get wrong |
| `epochSize` read from the hydration payload | No payload parameter: `w` is the verifier's | The composed root ignores `epochSize`, so a payload could change it without changing the root |
| A 16-ary MPT over hex paths; `mpt_prefix_verify` over `0x` hex | The binary trie over UTF-8 key bytes; `trie_prefix_verify` over `utf8(ns + "/")` | One trie for every structure; byte-exact prefixes |
| Entry values are JSON | Entry values are canonical values, committed by `commit(v)` | One data model; bytes and text never coincide |
| Projection-native: `CommittedView[S]` with a full-rebuild mode, an incremental mode and a `RootDivergence` assert | Batch-native core; the view is an optional adapter with an opt-in `verifyAgainst` | The trie is canonical in its key set, so there is one mode and nothing to diverge |
| `hashFor` and `advanceWork` with a 16-deep work cache | Drafts: a preview, chainable, committed as one write | Stateless and exact; metakit's cache was keyed by state root only |
| Ordinals dense: each the previous plus one | Versions strictly increasing; gaps allowed and provably absent | Applications choose their clock |
| Recommit idempotent or refused (any form) | Only the head, against its parent; identical means the same resulting state root | A retry is recognisable; a subset of the original batch is a rewrite |
| The journal written in separate steps | Every store in one compare-and-write | A crash between metakit's writes left the node unable to hydrate |
| Delta ring buffer of 64 | Deltas served down to the prune floor; below it "not retained" | Retention is one policy |
| Replica snapshot checked against its own roots | Checked against a head the caller already trusts | A self-consistent forgery passed metakit's check |
| Verify-gated hydration installs a snapshot into the live state | Restore writes a snapshot checked against a trusted head into an empty store, which is then the store pruned through that head | Persistent, atomic and checked against a trusted head; nothing before the head is fabricated |
| Snapshot contents checked once, whole | Checked chunk by chunk as they arrive, with ordering and size limits; roots checked at the end | A malformed, reordered or foreign snapshot is refused before it is all received |
| Entry values unbounded | A value is at most 1 MiB of canonical CBOR and 253 levels deep | Every entry fits one bounded snapshot chunk, and every delta and chunk stays within the canonical depth limit |
| The snapshot is served from one atomic cell | A snapshot is read in pieces and refused with a version conflict if a commit lands meanwhile | The store holds no in-memory copy; refusing is what keeps a snapshot from being torn |
| Historical key proofs not offered | A key at any retained version, through the history trie | Both tries are persistent |
| Reserved namespaces `fiber/`, `registry/`, `oracle/`, `meta/` | None | Applications own their namespaces |
| `CommitKey.unsafe` throws | Keys parse to a typed error | No exceptions in the library |
| An explicit null entry value | Deletes the key | Absent equals null, and a null cannot be proven present |
