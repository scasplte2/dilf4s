"""The proof verifiers' answers and versioned-trie records, as the native module builds them."""

from dataclasses import dataclass, field
from typing import Literal, final

from dilf4s._native import Value


@final
@dataclass(frozen=True, slots=True)
class Present:
    """A verified trie holds the key, with this value hash.

    Attributes:
        value_hash: The value hash the trie commits for the key.
    """

    value_hash: bytes
    kind: Literal["present"] = field(default="present", init=False)


@final
@dataclass(frozen=True, slots=True)
class Absent:
    """A verified trie proves the key absent."""

    kind: Literal["absent"] = field(default="absent", init=False)


@final
@dataclass(frozen=True, slots=True)
class NotCovered:
    """The proof does not reach the key or the whole listing: it establishes neither answer."""

    kind: Literal["notCovered"] = field(default="notCovered", init=False)


type Lookup = Present | Absent | NotCovered
"""What a verified trie says about one key."""

type Listing = tuple[tuple[bytes, bytes], ...] | NotCovered
"""Every `(key, value hash)` of a prefix or range in key order, complete, or `NotCovered()`."""


@final
@dataclass(frozen=True, slots=True)
class VersionedHead:
    """What one versioned-trie root commits to: the head version, its state root and the history root.

    Attributes:
        version: The head version: an `int` for an unsigned-long version key, `bytes` for a bytes key.
        state_root: The state trie's root at the head.
        history_root: The history trie's root, mapping every committed version to its state root.
    """

    version: int | bytes
    state_root: bytes
    history_root: bytes


@final
@dataclass(frozen=True, slots=True)
class Delta:
    """One version's change.

    Attributes:
        parent: The head before it, `None` for the first commit.
        head: The head after it.
        upserts: The keys written, with their values, in key order.
        removes: The keys removed.
    """

    parent: VersionedHead | None
    head: VersionedHead
    upserts: tuple[tuple[str, Value], ...]
    removes: tuple[str, ...]


@final
@dataclass(frozen=True, slots=True)
class HeadChunk:
    """A snapshot's first chunk: its head.

    Attributes:
        head: The head.
    """

    head: VersionedHead
    kind: Literal["head"] = field(default="head", init=False)


@final
@dataclass(frozen=True, slots=True)
class StateChunk:
    """Current entries, in strictly increasing key order across all state chunks.

    Attributes:
        entries: The entries.
    """

    entries: tuple[tuple[str, Value], ...]
    kind: Literal["state"] = field(default="state", init=False)


@final
@dataclass(frozen=True, slots=True)
class HistoryChunk:
    """Committed versions with their state roots, strictly increasing, ending with the head version.

    Attributes:
        versions: The `(version, state root)` pairs.
    """

    versions: tuple[tuple[int | bytes, bytes], ...]
    kind: Literal["history"] = field(default="history", init=False)


type SnapshotChunk = HeadChunk | StateChunk | HistoryChunk
"""One chunk of a snapshot stream."""
