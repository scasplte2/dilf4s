package core

// The entry points of the generated models through CBOR and JSON.

// FromCBOR is the typed value of canonical CBOR: the CanonicalError of Decode, or dec's DecodeFailure.
func FromCBOR[T any](b []byte, dec func(Value) (T, error)) (T, error) {
	var zero T
	if dec == nil {
		return zero, noDecoder()
	}
	v, err := Decode(b)
	if err != nil {
		return zero, err
	}
	return dec(v)
}

// FromJSON is the typed value of JSON text (canonical-encoding.md section 3): the CanonicalError of ParseJSON, or dec's
// DecodeFailure.
func FromJSON[T any](text string, dec func(Value) (T, error)) (T, error) {
	var zero T
	if dec == nil {
		return zero, noDecoder()
	}
	v, err := ParseJSON(text)
	if err != nil {
		return zero, err
	}
	return dec(v)
}

// ToCBOR is the canonical CBOR of x, encoded by enc from the zero Encoding (DefaultEncodingWeight): Encode of its
// value.
func ToCBOR[T any](x T, enc func(T, Encoding) (Value, error)) ([]byte, error) {
	if enc == nil {
		return nil, noEncoder()
	}
	v, err := enc(x, Encoding{})
	if err != nil {
		return nil, err
	}
	return Encode(v)
}

// ToCBORWithin is the canonical CBOR of x, encoded by enc, if it is at most maxBytes long (EncodeWithin's bounds):
// the model is encoded within a weight budget of the limit, as no encoding is shorter than its value's weight, so a
// model that shares its parts is refused as TooLarge before more than the limit is built. The budget is charged what
// the value weighs, so whether x fits depends on its value alone. The null members its maps drop are charged apart
// (Encoding): past their allowance, x is refused as TooLarge with the allowance as its limit.
func ToCBORWithin[T any](x T, enc func(T, Encoding) (Value, error), maxBytes int) ([]byte, error) {
	if enc == nil {
		return nil, noEncoder()
	}
	limit := byteLimit(maxBytes, MaxEncodedBytes)
	if limit == 0 {
		return nil, TooLarge{Limit: 0}
	}
	// a value past the budget is refused with the limit, the budget as applied
	v, err := enc(x, EncodingWithin(uint64(limit)))
	if err != nil {
		return nil, err
	}
	return EncodeWithin(v, limit)
}
