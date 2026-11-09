package core

import "hash"

// The commitment tags (docs/specs/merkle.md section 2).
const (
	// BytesTag leads the commitment of a byte string.
	BytesTag byte = 0x00
	// CBORTag leads the commitment of any other value.
	CBORTag byte = 0x01
)

// ValueCommitment is 0x00 || b for a byte string b, 0x01 || cbor(v) for any other value: the bytes a value commits to
// as a Merkle leaf or a trie value. The tag keeps the mapping injective: the bytes 0x6161 and the text "a" (whose CBOR
// is 0x6161) commit apart. ValueCommitmentWithin at MaxEncodedBytes, as Encode. To hash a commitment, use
// HashCommitment, which streams it.
func ValueCommitment(v Value) ([]byte, error) { return ValueCommitmentWithin(v, MaxEncodedBytes) }

// ValueCommitmentWithin is ValueCommitment, if it is at most maxBytes long (the tag included), with the bounds of
// EncodeWithin: O(min(weight, limit)), refused at once when the weight is over the limit, and never holding more than
// the limit. The refusal names the limit as applied.
func ValueCommitmentWithin(v Value, maxBytes int) ([]byte, error) {
	limit := byteLimit(maxBytes, MaxEncodedBytes)
	if v.Kind() == KindBytes {
		if len(v.n.bytes) >= limit {
			return nil, TooLarge{Limit: limit}
		}
		return append([]byte{BytesTag}, v.n.bytes...), nil
	}
	if limit == 0 {
		return nil, TooLarge{Limit: limit}
	}
	// the body is budgeted one byte short, for the tag; the refusal names the caller's limit
	body, err := EncodeWithin(v, limit-1)
	if err != nil {
		return nil, TooLarge{Limit: limit}
	}
	return append([]byte{CBORTag}, body...), nil
}

// HashCommitment is H(prefix[0] || prefix[1] || ... || commit(v)) under a: the commitment streamed into a hash this
// function creates, never built, with ValueCommitment's ceiling and refusal, checked before anything is hashed. A
// tagged hash passes [H(tag), H(tag)] as the prefix.
func HashCommitment(a HashAlgorithm, prefix [][]byte, v Value) ([]byte, error) {
	if err := a.check(); err != nil {
		return nil, err
	}
	if err := commitmentWithin(v, MaxEncodedBytes); err != nil {
		return nil, err
	}
	h := a.start()
	for _, p := range prefix {
		h.Write(p)
	}
	writeCommitment(h, v)
	return h.Sum(nil), nil
}

// WriteCommitment streams ValueCommitment's bytes into h without building them, refusing as ValueCommitmentWithin at
// maxBytes before h absorbs a byte.
func WriteCommitment(h hash.Hash, v Value, maxBytes int) error {
	if h == nil {
		return typeMismatch("expected a hash")
	}
	if err := commitmentWithin(v, byteLimit(maxBytes, MaxEncodedBytes)); err != nil {
		return err
	}
	writeCommitment(h, v)
	return nil
}

// commitmentWithin refuses as ValueCommitmentWithin at limit (already applied), without building anything.
func commitmentWithin(v Value, limit int) error {
	if v.Kind() == KindBytes {
		if len(v.n.bytes) >= limit {
			return TooLarge{Limit: limit}
		}
		return nil
	}
	if limit == 0 {
		return TooLarge{Limit: limit}
	}
	if _, err := EncodedLengthWithin(v, limit-1); err != nil {
		return TooLarge{Limit: limit}
	}
	return nil
}

func writeCommitment(h hash.Hash, v Value) {
	out := &staged{into: h}
	if v.Kind() == KindBytes {
		out.put([]byte{BytesTag})
		out.put(v.n.bytes)
	} else {
		out.put([]byte{CBORTag})
		writeValue(out, v)
	}
	out.flush()
}
