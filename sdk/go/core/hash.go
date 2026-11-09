package core

import (
	"crypto/sha256"
	"crypto/sha3"
	"crypto/sha512"
	"hash"
	"slices"

	"golang.org/x/crypto/blake2b"
	keccak "golang.org/x/crypto/sha3"
)

// HashAlgorithm is a hash function of docs/specs/canonical-encoding.md section 4, by the name the specs and vectors
// use. Only the seven constants are algorithms: any other value (a conversion from a string, or the zero value) is
// refused wherever it is used, as HashErrorUnknownAlgorithm, the refusal of ParseHashAlgorithm for that name.
type HashAlgorithm string

// The hash functions.
const (
	SHA256 HashAlgorithm = "SHA-256"
	SHA512 HashAlgorithm = "SHA-512"
	// SHA3_256 is SHA3-256 (FIPS 202).
	SHA3_256 HashAlgorithm = "SHA3-256"
	// SHA3_512 is SHA3-512 (FIPS 202).
	SHA3_512 HashAlgorithm = "SHA3-512"
	// Keccak256 is Keccak-256 as Ethereum uses it: the pre-standard padding, not SHA3-256's.
	Keccak256 HashAlgorithm = "Keccak-256"
	// BLAKE2b256 is BLAKE2b with a 32-byte output parameter (RFC 7693), not a truncated BLAKE2b-512.
	BLAKE2b256 HashAlgorithm = "BLAKE2b-256"
	// BLAKE2b512 is BLAKE2b-512 (RFC 7693).
	BLAKE2b512 HashAlgorithm = "BLAKE2b-512"
)

// HashAlgorithms is every hash function, in the specs' order: a fresh slice.
func HashAlgorithms() []HashAlgorithm {
	return []HashAlgorithm{SHA256, SHA512, SHA3_256, SHA3_512, Keccak256, BLAKE2b256, BLAKE2b512}
}

// ParseHashAlgorithm is the hash function named name, exactly as the specs write it, or HashErrorUnknownAlgorithm.
func ParseHashAlgorithm(name string) (HashAlgorithm, error) {
	a := HashAlgorithm(name)
	if !a.Valid() {
		return "", HashErrorUnknownAlgorithm{Name: PreviewOf(name)}
	}
	return a, nil
}

// Valid reports whether a is one of the seven hash functions.
func (a HashAlgorithm) Valid() bool { return slices.Contains(HashAlgorithms(), a) }

func (a HashAlgorithm) check() error {
	if !a.Valid() {
		return HashErrorUnknownAlgorithm{Name: PreviewOf(string(a))}
	}
	return nil
}

// OutputSize is the output size in bytes.
func (a HashAlgorithm) OutputSize() (int, error) {
	if err := a.check(); err != nil {
		return 0, err
	}
	return a.outputSize(), nil
}

func (a HashAlgorithm) outputSize() int {
	switch a {
	case SHA512, SHA3_512, BLAKE2b512:
		return 64
	}
	return 32
}

// BlockSize is the input block size in bytes (a sponge's rate): expand_message_xmd's s_in_bytes.
func (a HashAlgorithm) BlockSize() (int, error) {
	if err := a.check(); err != nil {
		return 0, err
	}
	return a.blockSize(), nil
}

func (a HashAlgorithm) blockSize() int {
	switch a {
	case SHA256:
		return 64
	case SHA3_512:
		return 72
	case SHA3_256, Keccak256:
		return 136
	}
	return 128
}

// ExpansionLimit is the most bytes ExpandMessageXmd expands to: min(255 * output size, 65535).
func (a HashAlgorithm) ExpansionLimit() (int, error) {
	if err := a.check(); err != nil {
		return 0, err
	}
	return a.expansionLimit(), nil
}

func (a HashAlgorithm) expansionLimit() int { return min(255*a.outputSize(), 65535) }

// StartHash is an incremental hash under a, with nothing absorbed: it holds a's state only.
func StartHash(a HashAlgorithm) (hash.Hash, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	return a.start(), nil
}

// start is a fresh hash of a, which is valid.
func (a HashAlgorithm) start() hash.Hash {
	switch a {
	case SHA512:
		return sha512.New()
	case SHA3_256:
		return sha3.New256()
	case SHA3_512:
		return sha3.New512()
	case Keccak256:
		return keccak.NewLegacyKeccak256()
	case BLAKE2b256:
		// an unkeyed BLAKE2b never refuses
		h, _ := blake2b.New256(nil)
		return h
	case BLAKE2b512:
		h, _ := blake2b.New512(nil)
		return h
	}
	return sha256.New()
}

// hashParts is the hash of the concatenation of parts under a, which is valid.
func (a HashAlgorithm) hashParts(parts ...[]byte) []byte {
	h := a.start()
	for _, p := range parts {
		h.Write(p)
	}
	return h.Sum(nil)
}

// HashParts is the hash of the concatenation of parts, each absorbed in turn (never copied whole).
func HashParts(a HashAlgorithm, parts ...[]byte) ([]byte, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	return a.hashParts(parts...), nil
}

// Digest is the hash of data.
func Digest(a HashAlgorithm, data []byte) ([]byte, error) { return HashParts(a, data) }

// DigestFromBytes is a copy of b, if it is exactly a's output size; HashErrorInvalidLength otherwise.
func DigestFromBytes(a HashAlgorithm, b []byte) ([]byte, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	if len(b) != a.outputSize() {
		return nil, HashErrorInvalidLength{Algorithm: string(a), Expected: uint64(a.outputSize()), Actual: uint64(len(b))}
	}
	return slices.Clone(b), nil
}

// DigestFromHex is a digest's bytes from its hex (canonical-encoding.md section 4): an optional 0x, then exactly twice
// a's output size in ASCII hex digits, either case. HashErrorInvalidHex at the first byte that is not a hex digit (or,
// for an odd number of digits, the text's length), HashErrorInvalidLength for any other number of digits; digits past
// the width are refused reading at most one past it, before the text is decoded, so the offset is the same in every
// language's string units (conformance.md section 8). The InvalidLength's actual counts bytes, as the Rust SDK's does.
func DigestFromHex(a HashAlgorithm, text string) ([]byte, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	width := a.outputSize()
	from, digits := 0, text
	if len(text) >= 2 && text[:2] == "0x" {
		from, digits = 2, text[2:]
	}
	span := 2 * width
	if len(digits) > span {
		for i := 0; i <= span; i++ {
			if _, ok := hexDigit(digits[i]); !ok {
				return nil, HashErrorInvalidHex{Offset: uint64(from + i)}
			}
		}
		return nil, HashErrorInvalidLength{Algorithm: string(a), Expected: uint64(width), Actual: uint64(len(digits)/2 + len(digits)%2)}
	}
	b, err := hexBytes(digits, from)
	if err != nil {
		return nil, err
	}
	if len(b) != width {
		return nil, HashErrorInvalidLength{Algorithm: string(a), Expected: uint64(width), Actual: uint64(len(b))}
	}
	return b, nil
}

// ExpandMessageXmd is RFC 9380 section 5.3.1 expand_message_xmd under a: length uniform bytes from the concatenation of
// message's parts, under the domain separation tag dst. A DST over 255 bytes is first reduced to
// H("H2C-OVERSIZE-DST-" || DST) (section 5.3.3). Refused as HashErrorInvalidExpansion, before anything is hashed, for
// an empty DST (emptyDst), then for a length outside 1..a.ExpansionLimit() (outputLength).
func ExpandMessageXmd(a HashAlgorithm, message [][]byte, dst []byte, length uint64) ([]byte, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	limit := a.expansionLimit()
	refused := func(r ExpansionRefusal) error {
		return HashErrorInvalidExpansion{Refusal: r, Algorithm: string(a), Limit: uint64(limit)}
	}
	if len(dst) == 0 {
		return nil, refused(ExpansionRefusalEmptyDst)
	}
	if length < 1 || length > uint64(limit) {
		return nil, refused(ExpansionRefusalOutputLength)
	}
	// the DST is read twice (its length, then its bytes): one copy, so both reads see the same bytes
	tag := slices.Clone(dst)
	if len(tag) > 255 {
		tag = a.hashParts([]byte("H2C-OVERSIZE-DST-"), tag)
	}
	suffix := append(tag, byte(len(tag)))
	parts := make([][]byte, 0, len(message)+3)
	parts = append(parts, make([]byte, a.blockSize()))
	parts = append(parts, message...)
	parts = append(parts, []byte{byte(length >> 8), byte(length), 0}, suffix)
	b0 := a.hashParts(parts...)
	bi := a.hashParts(b0, []byte{1}, suffix)
	out := slices.Clone(bi)
	size := a.outputSize()
	for i := 2; uint64(len(out)) < length; i++ {
		mixed := make([]byte, size)
		for j := range mixed {
			mixed[j] = b0[j] ^ bi[j]
		}
		bi = a.hashParts(mixed, []byte{byte(i)}, suffix)
		out = append(out, bi...)
	}
	return out[:length], nil
}

// DomainTag is a versioned domain-separation tag matching ^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$, such as
// dilf4s/mpt/leaf/v1. The zero DomainTag is no tag: it is refused wherever it is used, as InvalidDomainTag.
type DomainTag struct {
	tag string
}

// NewDomainTag is the tag, if it matches the grammar; HashErrorInvalidDomainTag otherwise.
func NewDomainTag(tag string) (DomainTag, error) {
	if !isDomainTag(tag) {
		return DomainTag{}, HashErrorInvalidDomainTag{Tag: PreviewOf(tag)}
	}
	return DomainTag{tag: tag}, nil
}

// String is the tag text.
func (t DomainTag) String() string { return t.tag }

func (t DomainTag) check() error {
	if !isDomainTag(t.tag) {
		return HashErrorInvalidDomainTag{Tag: PreviewOf(t.tag)}
	}
	return nil
}

// isDomainTag checks the grammar segment by segment: a first segment of [a-z0-9]+, middle segments of
// [a-z0-9][a-z0-9._-]*, and a last segment v[0-9]+; at least two segments.
func isDomainTag(tag string) bool {
	lowerDigit := func(b byte) bool { return (b >= 'a' && b <= 'z') || (b >= '0' && b <= '9') }
	var segments []string
	start := 0
	for i := 0; i <= len(tag); i++ {
		if i == len(tag) || tag[i] == '/' {
			segments = append(segments, tag[start:i])
			start = i + 1
		}
	}
	if len(segments) < 2 {
		return false
	}
	first, last := segments[0], segments[len(segments)-1]
	if first == "" {
		return false
	}
	for i := 0; i < len(first); i++ {
		if !lowerDigit(first[i]) {
			return false
		}
	}
	for _, s := range segments[1 : len(segments)-1] {
		if s == "" || !lowerDigit(s[0]) {
			return false
		}
		for i := 1; i < len(s); i++ {
			if c := s[i]; !lowerDigit(c) && c != '.' && c != '_' && c != '-' {
				return false
			}
		}
	}
	if len(last) < 2 || last[0] != 'v' {
		return false
	}
	for i := 1; i < len(last); i++ {
		if last[i] < '0' || last[i] > '9' {
			return false
		}
	}
	return true
}

// TaggedHash is taggedHash_H(tag, msg) = H(H(tag) || H(tag) || msg), the BIP-340 construction over any H. Building one
// hashes the tag once. The zero TaggedHash is no tagged hash: its methods refuse it as a TypeMismatch.
type TaggedHash struct {
	algorithm HashAlgorithm
	tag       DomainTag
	tagHash   []byte
}

// NewTaggedHash is the tagged hash for tag under a.
func NewTaggedHash(a HashAlgorithm, tag DomainTag) (TaggedHash, error) {
	if err := a.check(); err != nil {
		return TaggedHash{}, err
	}
	if err := tag.check(); err != nil {
		return TaggedHash{}, err
	}
	return TaggedHash{algorithm: a, tag: tag, tagHash: a.hashParts([]byte(tag.tag))}, nil
}

// Tag is the domain tag.
func (t TaggedHash) Tag() DomainTag { return t.tag }

// Algorithm is the hash function.
func (t TaggedHash) Algorithm() HashAlgorithm { return t.algorithm }

func (t TaggedHash) check() error {
	if t.tagHash == nil {
		return typeMismatch("a TaggedHash not made by NewTaggedHash")
	}
	return nil
}

// Start is a hash with the prefix H(tag) || H(tag) absorbed: absorbing a message, then summing, gives its tagged
// hash.
func (t TaggedHash) Start() (hash.Hash, error) {
	if err := t.check(); err != nil {
		return nil, err
	}
	return t.start(), nil
}

func (t TaggedHash) start() hash.Hash {
	h := t.algorithm.start()
	h.Write(t.tagHash)
	h.Write(t.tagHash)
	return h
}

// Hash is the tagged hash of message.
func (t TaggedHash) Hash(message []byte) ([]byte, error) {
	if err := t.check(); err != nil {
		return nil, err
	}
	h := t.start()
	h.Write(message)
	return h.Sum(nil), nil
}

// Content is the content hash of a canonical value: the tagged hash of its canonical encoding, streamed into the hash
// as it is written, never built whole. ContentWithin at MaxEncodedBytes: a value whose encoding is longer is TooLarge,
// as in the reference and the TypeScript SDK.
func (t TaggedHash) Content(v Value) ([]byte, error) { return t.ContentWithin(v, MaxEncodedBytes) }

// ContentWithin is Content, if v's encoding is at most maxBytes long (with EncodeWithin's bounds and refusal), checked
// before the hash absorbs a byte: from the weight when it is over, else measured.
func (t TaggedHash) ContentWithin(v Value, maxBytes int) ([]byte, error) {
	if err := t.check(); err != nil {
		return nil, err
	}
	if _, err := EncodedLengthWithin(v, maxBytes); err != nil {
		return nil, err
	}
	h := t.start()
	out := &staged{into: h}
	writeValue(out, v)
	out.flush()
	return h.Sum(nil), nil
}

// TaggedHashOf is taggedHash_a(tag, message).
func TaggedHashOf(a HashAlgorithm, tag DomainTag, message []byte) ([]byte, error) {
	t, err := NewTaggedHash(a, tag)
	if err != nil {
		return nil, err
	}
	return t.Hash(message)
}

// ContentHash is contentHash_a(tag, v) = taggedHash_a(tag, encode(v)), streamed (TaggedHash.Content).
func ContentHash(a HashAlgorithm, tag DomainTag, v Value) ([]byte, error) {
	t, err := NewTaggedHash(a, tag)
	if err != nil {
		return nil, err
	}
	return t.Content(v)
}

// WriteValue streams v's canonical encoding into h, as Encode would write it, without building it: memory is h's
// state and StagedBytes staged, never the encoding. It refuses, before h absorbs a byte, as EncodeWithin at maxBytes.
func WriteValue(h hash.Hash, v Value, maxBytes int) error {
	if h == nil {
		return typeMismatch("expected a hash")
	}
	if _, err := EncodedLengthWithin(v, maxBytes); err != nil {
		return err
	}
	out := &staged{into: h}
	writeValue(out, v)
	out.flush()
	return nil
}
