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
trie commits `valueHash(commit(v))`, so a JLVM program claims a value as itself, with a plain (non-raw) trie mode.

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
non-negative 64-bit integer as 8 bytes, which is exactly the JLVM's `int_to_bytes [v, 8]`. It is **not** the storage
key codec for integers (`storage.md` §1), which flips the sign bit and so would give a program a different key. `w` is
configuration, part of what a verifier trusts along with the root.

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
   be proven present anyway: in `trie_verify`, a null claims absence.
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
  version order. It is read in pieces; if a commit lands while it is read, it is refused rather than torn.
- **Replica.** A replica starts empty, or from a snapshot checked against a head the caller **already trusts**, never
  against the snapshot's own claim. The snapshot must rebuild the trusted state root and history root, its versions
  must be strictly increasing and end at the trusted head with its state root, and no entry may repeat. A replica
  applies a delta only if the delta's parent is its head, its version is greater, no key is both written and removed,
  and the head it recomputes (state root, then history root with the new version) is the delta's head.

## 8. Projection (informative)

The core takes batches; it never diffs states. An application whose state is a value `S` can supply a view: the
entries of `S`, and a delta between two states, which by default is the structural difference of their entries. A
hand-written delta is faster but can disagree with the entries; the opt-in check `Projection.verifyAgainst` rebuilds
the state root from the entries and compares it with the committed one.

## 9. Verifying in the JLVM

No operation is specific to versioned tries. A program uses `trie_verify`, `trie_prefix_verify`, `trie_range_verify`,
`int_to_bytes` and `cat` (`jlvm.md` §6), in plain (non-raw) modes. The trusted inputs are the root `R` and the width
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
| Historical key proofs not offered | A key at any retained version, through the history trie | Both tries are persistent |
| Reserved namespaces `fiber/`, `registry/`, `oracle/`, `meta/` | None | Applications own their namespaces |
| `CommitKey.unsafe` throws | Keys parse to a typed error | No exceptions in the library |
| An explicit null entry value | Deletes the key | Absent equals null, and a null cannot be proven present |
