package core

import (
	"math/big"
	"slices"
	"strconv"
	"strings"
)

// The runtime of the models generated from shared/schemas (core/generated): both directions between typed data and
// the canonical data model, with the same shape checks, so an encoder refuses every value its decoder would reject and
// a schema-described type never encodes to bytes outside its schema.
//
// An encoder is a func(T, Encoding) (Value, error), a decoder a func(Value) (T, error). Encoders refuse as
// CanonicalError kinds (TypeMismatch for a value outside its schema, DepthExceeded past MaxDepth) or as TooLarge (past
// the encoding's weight budget); decoders as a DecodeFailure, its path from the root. Containers refuse to go past
// MaxDepth before descending, so a model nested past the limit (a Go value can nest without bound) is a typed
// DepthExceeded, never a stack overflow; decoding reads a Value, which never nests past MaxDepth, one call per level.

// DefaultEncodingWeight is the weight budget of the zero Encoding: MaxProofWeight (2^24), the bound every verifier
// puts on its input. A Go model can share its parts (a slice, a pointer, an interface holding the same value twice),
// so a model of a few hundred bytes can stand for a value of 2^60 nodes, which encoding would build one node at a
// time: the budget refuses it, as TooLarge, before more than the budget is built. Encode a heavier model within a
// budget of its own (EncodingWithin, ToCBORWithin).
const DefaultEncodingWeight = MaxProofWeight

// Encoding is where a model is being encoded: its nesting level (0 at the root), and its weight budget, the most the
// canonical value it builds may weigh (Value.Weight). Every node the value keeps is charged against the budget as it
// is built, so whether a model fits the budget depends on its value alone, never on how the model spells it. A map's
// null member, which the value leaves out, is charged apart with its key, against an allowance of
// DefaultEncodingWeight (or the budget, if larger), past which the encode is refused as TooLarge too: so the work and
// memory of encoding are bounded, however much the model shares. The zero Encoding is the root, with
// DefaultEncodingWeight. A root Encoding may be reused: each encode that starts from it counts its own.
type Encoding struct {
	level int
	limit uint64
	tally *tally
}

// tally is what one encode has built: the weight its value keeps, and the weight of the null members (and their keys)
// its maps have dropped.
type tally struct{ kept, dropped uint64 }

// EncodingWithin is the root of an encoding whose value may weigh at most maxWeight; 0 is DefaultEncodingWeight.
func EncodingWithin(maxWeight uint64) Encoding { return Encoding{limit: maxWeight} }

// Level is the nesting level: 0 at the root.
func (e Encoding) Level() int { return e.level }

func (e Encoding) budget() uint64 {
	if e.limit == 0 {
		return DefaultEncodingWeight
	}
	return e.limit
}

// tooLarge is the refusal of a value past the budget, its limit the budget as applied.
func (e Encoding) tooLarge() error {
	return TooLarge{Limit: int(min(e.budget(), uint64(MaxEncodedBytes)))}
}

// charge counts w more of the value's weight, refusing past the budget.
func (e Encoding) charge(w uint64) error {
	if !e.room(w) {
		return e.tooLarge()
	}
	if e.tally != nil {
		e.tally.kept += w
	}
	return nil
}

// room reports whether n more of weight fit the budget, charging nothing.
func (e Encoding) room(n uint64) bool {
	kept := uint64(0)
	if e.tally != nil {
		kept = e.tally.kept
	}
	return n <= e.budget() && kept <= e.budget()-n
}

// drop counts w more of the weight a map has dropped (its null members and their keys), refusing past the allowance:
// DefaultEncodingWeight, or the budget if larger.
func (e Encoding) drop(w uint64) error {
	allowance, dropped := max(e.budget(), DefaultEncodingWeight), uint64(0)
	if e.tally != nil {
		dropped = e.tally.dropped
	}
	if w > allowance || dropped > allowance-w {
		return TooLarge{Limit: int(min(allowance, uint64(MaxEncodedBytes)))}
	}
	if e.tally != nil {
		e.tally.dropped += w
	}
	return nil
}

// text is the text leaf build makes of s, refused by its length (a text weighs its length and one) before build
// validates it, so a text past the budget costs no more than the check.
func (e Encoding) text(s string, build func(string) (Value, error)) (Value, error) {
	if !e.room(sizedWeight(len(s))) {
		return Value{}, e.tooLarge()
	}
	return e.built(build(s))
}

// built is a leaf v, charged by its weight, or err. A null is charged by its container, which keeps it (an array) or
// drops it (a map).
func (e Encoding) built(v Value, err error) (Value, error) {
	if err != nil {
		return Value{}, err
	}
	if v.IsNull() {
		return v, nil
	}
	if err := e.charge(v.Weight()); err != nil {
		return Value{}, err
	}
	return v, nil
}

// Nested is the encoding inside a container at e, if a container fits there: CanonicalErrorDepthExceeded at
// MaxDepth. The first container of an encode starts its count of the weight built.
func (e Encoding) Nested() (Encoding, error) {
	if e.level >= MaxDepth {
		return Encoding{}, depthExceeded()
	}
	if e.tally == nil {
		e.tally = new(tally)
	}
	return Encoding{level: e.level + 1, limit: e.limit, tally: e.tally}, nil
}

// noEncoder and noDecoder refuse a codec built without its function (a nil func), which is never called.
func noEncoder() error { return typeMismatch("a codec without an encoder") }

func noDecoder() error { return Failure("a codec without a decoder") }

func expected(what string, v Value) error {
	return Failure("expected " + what + ", got " + v.Kind().String())
}

func showMax(hi int) string {
	if hi < 0 {
		return ""
	}
	return strconv.Itoa(hi)
}

// ------------------------------------------------------------------------------------------------ leaves

// EncodeAny is v (CDDL any).
func EncodeAny(v Value, at Encoding) (Value, error) { return at.built(v, nil) }

// DecodeAny is v (CDDL any).
func DecodeAny(v Value) (Value, error) { return v, nil }

// EncodeBool is the boolean b.
func EncodeBool(b bool, at Encoding) (Value, error) { return at.built(Bool(b), nil) }

// DecodeBool is the boolean v.
func DecodeBool(v Value) (bool, error) {
	if b, ok := v.AsBool(); ok {
		return b, nil
	}
	return false, expected("bool", v)
}

// EncodeText is the text s: CanonicalErrorInvalidUtf8 when s is not well-formed UTF-8.
func EncodeText(s string, at Encoding) (Value, error) { return at.text(s, Text) }

// DecodeText is the text v.
func DecodeText(v Value) (string, error) {
	if s, ok := v.AsText(); ok {
		return s, nil
	}
	return "", expected("text", v)
}

// EncodeBytes is the byte string holding a copy of b.
func EncodeBytes(b []byte, at Encoding) (Value, error) {
	// refused by its length before it is copied
	if err := at.charge(sizedWeight(len(b))); err != nil {
		return Value{}, err
	}
	return Bytes(b), nil
}

// DecodeBytes is a copy of the byte string v.
func DecodeBytes(v Value) ([]byte, error) {
	if b, ok := v.AsBytes(); ok {
		return b, nil
	}
	return nil, expected("bytes", v)
}

// encodeSizedBytes is a copy of b, if it is lo to hi bytes long (hi negative: unbounded).
func encodeSizedBytes(b []byte, lo, hi int) (Value, error) {
	if len(b) < lo || (hi >= 0 && len(b) > hi) {
		return Value{}, typeMismatch("byte string of " + strconv.Itoa(len(b)) + " bytes outside the schema size")
	}
	return Bytes(b), nil
}

// decodeSizedBytes is a copy of the byte string v, if it is lo to hi bytes long (hi negative: unbounded).
func decodeSizedBytes(v Value, lo, hi int) ([]byte, error) {
	if v.Kind() != KindBytes {
		return nil, expected("bytes", v)
	}
	n := len(v.n.bytes)
	if n < lo || (hi >= 0 && n > hi) {
		return nil, Failure("byte string of " + strconv.Itoa(n) + " bytes, expected " + strconv.Itoa(lo) + ".." + showMax(hi))
	}
	return slices.Clone(v.n.bytes), nil
}

// DecodeFixedBytes copies the byte string v into out, if it is exactly len(out) bytes long: a generated model holds a
// byte string of one size as an array, which is copied by value and so never aliased.
func DecodeFixedBytes(v Value, out []byte) error {
	if v.Kind() != KindBytes {
		return expected("bytes", v)
	}
	if len(v.n.bytes) != len(out) {
		return Failure("byte string of " + strconv.Itoa(len(v.n.bytes)) + " bytes, expected " + strconv.Itoa(len(out)))
	}
	copy(out, v.n.bytes)
	return nil
}

// encodeSizedText is the text s, if its UTF-8 encoding is lo to hi bytes long (hi negative: unbounded).
func encodeSizedText(s string, lo, hi int) (Value, error) {
	t, err := Text(s)
	if err != nil {
		return Value{}, err
	}
	if len(s) < lo || (hi >= 0 && len(s) > hi) {
		return Value{}, typeMismatch("text of " + strconv.Itoa(len(s)) + " UTF-8 bytes outside the schema size")
	}
	return t, nil
}

// decodeSizedText is the text v, if its UTF-8 encoding is lo to hi bytes long (hi negative: unbounded).
func decodeSizedText(v Value, lo, hi int) (string, error) {
	s, ok := v.AsText()
	if !ok {
		return "", expected("text", v)
	}
	if len(s) < lo || (hi >= 0 && len(s) > hi) {
		return "", Failure("text of " + strconv.Itoa(len(s)) + " UTF-8 bytes, expected " + strconv.Itoa(lo) + ".." + showMax(hi))
	}
	return s, nil
}

// TextPattern is one of the .regexp patterns the schemas use: every SDK checks them identically, without a
// regular-expression engine.
type TextPattern uint8

// The patterns of the profile (models-gen's Profile.Patterns).
const (
	// PatternHex is ([0-9a-f]{2})*: lowercase hex of whole bytes.
	PatternHex TextPattern = iota + 1
	// PatternHex0x is 0x([0-9a-f]{2})*: the same, 0x-prefixed.
	PatternHex0x
	// PatternDecimal is 0|[1-9][0-9]*: a non-negative decimal with no leading zero.
	PatternDecimal
	// PatternStateKey is [a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*: a versioned-trie state key's grammar
	// (versioned-trie.md section 2), without the length limits the domain checks.
	PatternStateKey
)

// Source is the pattern as the schema writes it.
func (p TextPattern) Source() string {
	switch p {
	case PatternHex:
		return "([0-9a-f]{2})*"
	case PatternHex0x:
		return "0x([0-9a-f]{2})*"
	case PatternDecimal:
		return "0|[1-9][0-9]*"
	case PatternStateKey:
		return "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*"
	}
	return "<no pattern>"
}

// Matches reports whether the whole of text matches the pattern; nothing matches a value that is no pattern.
func (p TextPattern) Matches(text string) bool {
	lowerHex := func(s string) bool {
		if len(s)%2 != 0 {
			return false
		}
		for i := 0; i < len(s); i++ {
			if c := s[i]; !(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') {
				return false
			}
		}
		return true
	}
	switch p {
	case PatternHex:
		return lowerHex(text)
	case PatternHex0x:
		return strings.HasPrefix(text, "0x") && lowerHex(text[2:])
	case PatternDecimal:
		if text == "0" {
			return true
		}
		if text == "" || text[0] == '0' {
			return false
		}
		for i := 0; i < len(text); i++ {
			if text[i] < '0' || text[i] > '9' {
				return false
			}
		}
		return true
	case PatternStateKey:
		// one pass, allocating nothing: start is the first byte of the segment i is in
		start := 0
		for i := 0; i <= len(text); i++ {
			if i == len(text) || text[i] == '/' {
				if i == start {
					return false
				}
				start = i + 1
				continue
			}
			c := text[i]
			lowerDigit := (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
			if !lowerDigit && (i == start || (c != '.' && c != '_' && c != '-')) {
				return false
			}
		}
		return true
	}
	return false
}

// encodeMatching is the text s, if it matches p.
func encodeMatching(s string, p TextPattern) (Value, error) {
	if !p.Matches(s) {
		return Value{}, typeMismatch("text does not match " + p.Source())
	}
	return Text(s)
}

// decodeMatching is the text v, if it matches p.
func decodeMatching(v Value, p TextPattern) (string, error) {
	s, ok := v.AsText()
	if !ok {
		return "", expected("text", v)
	}
	if !p.Matches(s) {
		return "", Failure(PreviewOf(s).String() + " does not match " + p.Source())
	}
	return s, nil
}

// EncodeLiteralText is exactly the text lit: a field holding a literal carries no data, so its model leaves it out
// and its codec writes and checks it.
func EncodeLiteralText(lit string, at Encoding) (Value, error) { return at.built(Text(lit)) }

// DecodeLiteralText checks that v is exactly the text lit.
func DecodeLiteralText(v Value, lit string) error {
	s, ok := v.AsText()
	if !ok {
		return expected("text", v)
	}
	if s != lit {
		return Failure("expected " + strconv.Quote(lit) + ", got " + PreviewOf(s).String())
	}
	return nil
}

// EncodeLiteralInt is exactly the integer lit.
func EncodeLiteralInt(lit int64, at Encoding) (Value, error) { return at.built(Int64(lit), nil) }

// DecodeLiteralInt checks that v is exactly the integer lit.
func DecodeLiteralInt(v Value, lit int64) error {
	i, ok := v.AsInt()
	if !ok {
		return expected("int", v)
	}
	if n, fits := i.Int64(); !fits || n != lit {
		return Failure("expected " + strconv.FormatInt(lit, 10) + ", got an integer of " + strconv.FormatUint(i.bits, 10) + " bits")
	}
	return nil
}

// LiteralText is the decoder of a field holding exactly the text lit.
func LiteralText(lit string) func(Value) (struct{}, error) {
	return func(v Value) (struct{}, error) { return struct{}{}, DecodeLiteralText(v, lit) }
}

// LiteralInt is the decoder of a field holding exactly the integer lit.
func LiteralInt(lit int64) func(Value) (struct{}, error) {
	return func(v Value) (struct{}, error) { return struct{}{}, DecodeLiteralInt(v, lit) }
}

// ------------------------------------------------------------------------------------------------ integers

func outsideRange(bits uint64, lo, hi string) string {
	return "integer of " + strconv.FormatUint(bits, 10) + " bits outside " + lo + ".." + hi
}

// encodeUint64In is x, if it is in lo..hi.
func encodeUint64In(x, lo, hi uint64) (Value, error) {
	if x < lo || x > hi {
		return Value{}, typeMismatch(outsideRange(bitLength(new(big.Int).SetUint64(x)), strconv.FormatUint(lo, 10), strconv.FormatUint(hi, 10)))
	}
	return Uint64(x), nil
}

// decodeUint64In is the integer v, if it is in lo..hi.
func decodeUint64In(v Value, lo, hi uint64) (uint64, error) {
	i, ok := v.AsInt()
	if !ok {
		return 0, expected("int", v)
	}
	if n, fits := i.Uint64(); fits && n >= lo && n <= hi {
		return n, nil
	}
	return 0, Failure(outsideRange(i.bits, strconv.FormatUint(lo, 10), strconv.FormatUint(hi, 10)))
}

// encodeUint32In is x, if it is in lo..hi.
func encodeUint32In(x, lo, hi uint32) (Value, error) {
	return encodeUint64In(uint64(x), uint64(lo), uint64(hi))
}

// decodeUint32In is the integer v, if it is in lo..hi.
func decodeUint32In(v Value, lo, hi uint32) (uint32, error) {
	n, err := decodeUint64In(v, uint64(lo), uint64(hi))
	return uint32(n), err
}

// encodeInt64In is x, if it is in lo..hi.
func encodeInt64In(x, lo, hi int64) (Value, error) {
	if x < lo || x > hi {
		return Value{}, typeMismatch(outsideRange(bitLength(big.NewInt(x)), strconv.FormatInt(lo, 10), strconv.FormatInt(hi, 10)))
	}
	return Int64(x), nil
}

// decodeInt64In is the integer v, if it is in lo..hi.
func decodeInt64In(v Value, lo, hi int64) (int64, error) {
	i, ok := v.AsInt()
	if !ok {
		return 0, expected("int", v)
	}
	if n, fits := i.Int64(); fits && n >= lo && n <= hi {
		return n, nil
	}
	return 0, Failure(outsideRange(i.bits, strconv.FormatInt(lo, 10), strconv.FormatInt(hi, 10)))
}

// encodeInt32In is x, if it is in lo..hi.
func encodeInt32In(x, lo, hi int32) (Value, error) {
	return encodeInt64In(int64(x), int64(lo), int64(hi))
}

// decodeInt32In is the integer v, if it is in lo..hi.
func decodeInt32In(v Value, lo, hi int32) (int32, error) {
	n, err := decodeInt64In(v, int64(lo), int64(hi))
	return int32(n), err
}

// encodeIntIn is x, if it is in lo..hi: the range first (TypeMismatch), as every Int is within MaxIntegerBits.
func encodeIntIn(x Int, lo, hi BigBound) (Value, error) {
	l, h := lo.value(), hi.value()
	if x.big().Cmp(l) < 0 || x.big().Cmp(h) > 0 {
		return Value{}, typeMismatch(outsideRange(x.bits, l.String(), h.String()))
	}
	return IntValue(x), nil
}

// decodeIntIn is the integer v, if it is in lo..hi.
func decodeIntIn(v Value, lo, hi BigBound) (Int, error) {
	i, ok := v.AsInt()
	if !ok {
		return Int{}, expected("int", v)
	}
	l, h := lo.value(), hi.value()
	if i.big().Cmp(l) < 0 || i.big().Cmp(h) > 0 {
		return Int{}, Failure(outsideRange(i.bits, l.String(), h.String()))
	}
	return i, nil
}

// intBounds are the bounds of CDDL's int (RFC 8610): uint is major type 0 (0..2^64-1), nint major type 1
// (-2^64..-1), int either (-2^64..2^64-1). A wider integer is an explicit range in the schema.
func intBounds() (BigBound, BigBound) {
	return BigBound{Negative: true, Magnitude: "\x01\x00\x00\x00\x00\x00\x00\x00\x00"},
		BigBound{Magnitude: "\xff\xff\xff\xff\xff\xff\xff\xff"}
}

// EncodeInt is x, if it is a CDDL int: -2^64..2^64-1.
func EncodeInt(x Int, at Encoding) (Value, error) {
	lo, hi := intBounds()
	return at.built(encodeIntIn(x, lo, hi))
}

// DecodeInt is the integer v, if it is a CDDL int.
func DecodeInt(v Value) (Int, error) {
	lo, hi := intBounds()
	return decodeIntIn(v, lo, hi)
}

// EncodeNint is x, if it is a CDDL nint: -2^64..-1.
func EncodeNint(x Int, at Encoding) (Value, error) {
	lo, _ := intBounds()
	return at.built(encodeIntIn(x, lo, BigBound{Negative: true, Magnitude: "\x01"}))
}

// DecodeNint is the integer v, if it is a CDDL nint.
func DecodeNint(v Value) (Int, error) {
	lo, _ := intBounds()
	return decodeIntIn(v, lo, BigBound{Negative: true, Magnitude: "\x01"})
}

// EncodeUint is x, a CDDL uint: every uint64 is one.
func EncodeUint(x uint64, at Encoding) (Value, error) { return at.built(Uint64(x), nil) }

// DecodeUint is the integer v, if it is a CDDL uint.
func DecodeUint(v Value) (uint64, error) { return decodeUint64In(v, 0, 1<<64-1) }

// ------------------------------------------------------------------------------------------------ choices

// EncodeTextChoice is the text of x, a member of a closed choice of texts named name, if valid says it is one: a Go
// string type admits any string by conversion, so the encoder checks membership.
func EncodeTextChoice[T ~string](x T, valid bool, name string, at Encoding) (Value, error) {
	if !valid {
		return Value{}, typeMismatch(PreviewOf(string(x)).String() + " is not a " + name)
	}
	return at.built(Text(string(x)))
}

// DecodeTextChoice is the member of a closed choice of texts that v is, if valid says it is one.
func DecodeTextChoice[T ~string](v Value, valid func(T) bool) (T, error) {
	if valid == nil {
		return "", noDecoder()
	}
	s, ok := v.AsText()
	if !ok {
		return "", Failure("expected one of the text alternatives, got " + v.Kind().String())
	}
	if !valid(T(s)) {
		return "", Failure(PreviewOf(s).String() + " is not an alternative")
	}
	return T(s), nil
}

// EncodeEnum is wire, the integer a case of a labelled integer enumeration named name is written as, if valid says
// the case is one: a Go integer type admits any value by conversion, so the encoder checks membership.
func EncodeEnum(wire Int, valid bool, name string, at Encoding) (Value, error) {
	if !valid {
		return Value{}, typeMismatch("not a case of " + name)
	}
	return at.built(IntValue(wire), nil)
}

// DecodeEnum is the case of all (a labelled integer enumeration named name) written as v, by each case's wire.
func DecodeEnum[T comparable](v Value, all []T, wire func(T) Int, name string) (T, error) {
	var none T
	if wire == nil {
		return none, noDecoder()
	}
	i, ok := v.AsInt()
	if !ok {
		return none, expected("int", v)
	}
	for _, c := range all {
		if wire(c).Equal(i) {
			return c, nil
		}
	}
	return none, Failure("an integer of " + strconv.FormatUint(i.bits, 10) + " bits is not a " + name)
}

// Keys are a record alternative's required and allowed wire keys, which tell it apart when decoding.
type Keys struct {
	Required []string
	Allowed  []string
}

// SelectAlternative is the index of the alternative of a choice that v fits: the first record (given by its keys)
// whose required keys are all present and which allows every key present, for a map; the text alternatives, numbered
// after the records, for text. The generator only accepts choices where at most one record fits.
func SelectAlternative(v Value, records []Keys, texts []string) (int, error) {
	switch v.Kind() {
	case KindMap:
		for i, r := range records {
			fits := true
			for _, k := range r.Required {
				if _, ok := v.Get(k); !ok {
					fits = false
					break
				}
			}
			for _, k := range v.n.keys {
				if !fits {
					break
				}
				fits = slices.Contains(r.Allowed, k)
			}
			if fits {
				return i, nil
			}
		}
		return 0, Failure("no alternative fits a map of " + strconv.Itoa(len(v.n.keys)) + " keys")
	case KindText:
		if i := slices.Index(texts, v.n.text); i >= 0 {
			return len(records) + i, nil
		}
		return 0, Failure(PreviewOf(v.n.text).String() + " is not an alternative")
	}
	return 0, Failure("expected a map or one of the text alternatives")
}

// UnknownAlternative is the refusal of an alternative index a generated decoder does not know: unreachable while the
// generated code is current.
func UnknownAlternative(i int) error { return Failure("unknown alternative " + strconv.Itoa(i)) }

// NoAlternative is the refusal to encode a choice named name holding no alternative (nil, or a type that is not one).
func NoAlternative(name string) error { return typeMismatch("no alternative of " + name + " fits") }

// ------------------------------------------------------------------------------------------------ containers

// encodeVec is the array of xs, each encoded by enc one level down, if it has min to max elements (max negative:
// unbounded).
func encodeVec[T any](xs []T, lo, hi int, at Encoding, enc func(T, Encoding) (Value, error)) (Value, error) {
	inner, err := at.Nested()
	if err != nil {
		return Value{}, err
	}
	if len(xs) < lo || (hi >= 0 && len(xs) > hi) {
		return Value{}, typeMismatch("array of " + strconv.Itoa(len(xs)) + " elements outside the schema size")
	}
	// the array's own node; each element weighs at least 1, so an array of more than the budget left is refused in
	// O(1), before anything is allocated (a slice of a zero-size type may be any length in no memory)
	if err := inner.charge(1); err != nil {
		return Value{}, err
	}
	if !inner.room(uint64(len(xs))) {
		return Value{}, inner.tooLarge()
	}
	// grown as the elements are built, each charged first (a null, which a leaf leaves to its container, here): the
	// slots never outrun the budget
	items := make([]Value, 0, min(len(xs), 1024))
	for _, x := range xs {
		item, err := enc(x, inner)
		if err != nil {
			return Value{}, err
		}
		if item.IsNull() {
			if err := inner.charge(1); err != nil {
				return Value{}, err
			}
		}
		items = append(items, item)
	}
	return arrayOf(items)
}

// decodeVec is the elements of the array v, each decoded by dec, if it has min to max elements (max negative:
// unbounded).
func decodeVec[T any](v Value, lo, hi int, dec func(Value) (T, error)) ([]T, error) {
	if v.Kind() != KindArray {
		return nil, expected("array", v)
	}
	n := len(v.n.items)
	if n < lo || (hi >= 0 && n > hi) {
		return nil, Failure("array of " + strconv.Itoa(n) + " elements, expected " + strconv.Itoa(lo) + ".." + showMax(hi))
	}
	// grown from nothing as the elements decode, never ahead of them: an element type may be far larger than the value
	// it decodes from (a fixed byte array from a null), so no slot is made for an element that has not decoded
	out := []T{}
	for i, item := range v.n.items {
		x, err := dec(item)
		if err != nil {
			return nil, atIndex(err, i)
		}
		out = append(out, x)
	}
	return out, nil
}

// encodeTextMap is the map of m (CDDL { * text => T }), each value encoded by enc one level down, in canonical key
// order, the order m holds them in: a refusal is the first in that order.
func encodeTextMap[T any](m TextMap[T], at Encoding, enc func(T, Encoding) (Value, error)) (Value, error) {
	inner, err := at.Nested()
	if err != nil {
		return Value{}, err
	}
	// the map's own node, then each key once its value is encoded: kept with it, or dropped with a null. A key is
	// charged by its length before it is validated, as a text is (a dropped one too, as Map validates every key).
	if err := inner.charge(1); err != nil {
		return Value{}, err
	}
	ks := make([]string, 0, min(len(m.keys), 1024))
	vs := make([]Value, 0, min(len(m.keys), 1024))
	for i, k := range m.keys {
		e, err := enc(m.vals[i], inner)
		if err != nil {
			return Value{}, err
		}
		if e.IsNull() {
			err = inner.drop(addWeight(sizedWeight(len(k)), 1))
		} else {
			err = inner.charge(sizedWeight(len(k)))
		}
		if err != nil {
			return Value{}, err
		}
		if at, bad := invalidUTF8(k); bad {
			return Value{}, CanonicalErrorInvalidUtf8{Offset: uint64(at)}
		}
		if !e.IsNull() {
			ks, vs = append(ks, k), append(vs, e)
		}
	}
	return mapOfSorted(ks, vs)
}

// decodeTextMap is the entries of the map v, each value decoded by dec, in canonical key order: a TextMap, which
// compares its keys and never hashes them, as they are the value's (a caller's or a peer's).
func decodeTextMap[T any](v Value, dec func(Value) (T, error)) (TextMap[T], error) {
	if v.Kind() != KindMap {
		return TextMap[T]{}, expected("map", v)
	}
	// the values grown as they decode, never ahead of them, as decodeVec's elements are
	vals := []T{}
	for i, k := range v.n.keys {
		x, err := dec(v.n.vals[i])
		if err != nil {
			return TextMap[T]{}, at(err, k)
		}
		vals = append(vals, x)
	}
	return TextMap[T]{keys: slices.Clone(v.n.keys), vals: vals}, nil
}

// EncodeOptional is the value of o encoded by enc at depth, when present.
func EncodeOptional[T any](o Optional[T], at Encoding, enc func(T, Encoding) (Value, error)) (Optional[Value], error) {
	if enc == nil {
		return None[Value](), noEncoder()
	}
	x, ok := o.Get()
	if !ok {
		return None[Value](), nil
	}
	e, err := enc(x, at)
	if err != nil {
		return None[Value](), err
	}
	return Some(e), nil
}

// encodePointer is *p encoded by enc at depth: a field holding its own record by value is a pointer, and a nil one is
// a TypeMismatch.
func encodePointer[T any](p *T, at Encoding, enc func(T, Encoding) (Value, error)) (Value, error) {
	if p == nil {
		return Value{}, typeMismatch("a nil pointer where the schema requires a value")
	}
	return enc(*p, at)
}

// decodePointer is a pointer to the value dec decodes.
func decodePointer[T any](v Value, dec func(Value) (T, error)) (*T, error) {
	x, err := dec(v)
	if err != nil {
		return nil, err
	}
	return &x, nil
}

// ------------------------------------------------------------------------------------------------ records

// Field is one entry of a record being encoded: absent for an optional field with no value, never written as null.
type Field struct {
	key     string
	value   Value
	present bool
}

// Req is a required field's entry.
func Req(key string, v Value) Field { return Field{key: key, value: v, present: true} }

// Opt is an optional field's entry, absent when v is.
func Opt(key string, v Optional[Value]) Field {
	x, ok := v.Get()
	return Field{key: key, value: x, present: ok}
}

// Record is the map of a record's fields (a null value dropped, as every map drops one), charging the record's own
// node and the keys it keeps against e, the encoding inside it (its fields' values are charged as they are built),
// and a dropped null with its key against e's allowance for them.
func (e Encoding) Record(fields ...Field) (Value, error) {
	own, dropped := uint64(1), uint64(0)
	for _, f := range fields {
		switch {
		case !f.present:
		case f.value.IsNull():
			dropped = addWeight(dropped, addWeight(sizedWeight(len(f.key)), 1))
		default:
			own = addWeight(own, sizedWeight(len(f.key)))
		}
	}
	if err := e.charge(own); err != nil {
		return Value{}, err
	}
	if dropped > 0 {
		if err := e.drop(dropped); err != nil {
			return Value{}, err
		}
	}
	entries := make([]Entry, 0, len(fields))
	for _, f := range fields {
		if f.present {
			entries = append(entries, Entry{Key: f.key, Value: f.value})
		}
	}
	return Map(entries...)
}

// Fields are the entries of a record being decoded: a map with no keys beyond those allowed.
type Fields struct {
	m Value
}

// RecordFields is the fields of v, which must be a map whose keys are all in allowed. A refusal counts the unknown keys
// and previews the first in canonical order, in one pass; it never lists them.
func RecordFields(v Value, allowed ...string) (Fields, error) {
	if v.Kind() != KindMap {
		return Fields{}, expected("map", v)
	}
	count, first := 0, ""
	for _, k := range v.n.keys {
		if !slices.Contains(allowed, k) {
			if count == 0 {
				first = k
			}
			count++
		}
	}
	if count > 0 {
		noun := "keys"
		if count == 1 {
			noun = "key"
		}
		return Fields{}, Failure(strconv.Itoa(count) + " unknown " + noun + ", the first " + PreviewOf(first).String())
	}
	return Fields{m: v}, nil
}

// Required is the value at key decoded by dec, which must be present.
func Required[T any](f Fields, key string, dec func(Value) (T, error)) (T, error) {
	if dec == nil {
		var zero T
		return zero, noDecoder()
	}
	v, ok := f.m.Get(key)
	if !ok {
		var zero T
		return zero, Failure("missing required key " + strconv.Quote(key))
	}
	x, err := dec(v)
	if err != nil {
		return x, at(err, key)
	}
	return x, nil
}

// OptionalField is the value at key decoded by dec, if present.
func OptionalField[T any](f Fields, key string, dec func(Value) (T, error)) (Optional[T], error) {
	if dec == nil {
		return None[T](), noDecoder()
	}
	v, ok := f.m.Get(key)
	if !ok {
		return None[T](), nil
	}
	x, err := dec(v)
	if err != nil {
		return None[T](), at(err, key)
	}
	return Some(x), nil
}

// ------------------------------------------------------------------------------------------------ entry points

// DecodeWithin is dec(v), if v weighs at most maxWeight: a heavier value is refused in O(1), from its cached weight,
// before any of it is read or copied. Decode a caller's value with this: decoding walks a value as the schema reads it,
// so a value built with shared parts costs up to its weight.
func DecodeWithin[T any](v Value, maxWeight uint64, dec func(Value) (T, error)) (T, error) {
	var zero T
	if dec == nil {
		return zero, noDecoder()
	}
	if v.Weight() > maxWeight {
		return zero, Failure("the value weighs over " + strconv.FormatUint(maxWeight, 10))
	}
	return dec(v)
}

// ------------------------------------------------------------------------------------------------ codecs

// The codecs generated code composes: each a value whose Encode and Decode methods are an encoder and a decoder, so a
// field's codec is one expression (core.Uint64In{Lo: 0, Hi: 255}.Decode).

// Uint64In is an integer in Lo..Hi, as a uint64.
type Uint64In struct{ Lo, Hi uint64 }

// Encode is x, if it is in range.
func (c Uint64In) Encode(x uint64, at Encoding) (Value, error) {
	return at.built(encodeUint64In(x, c.Lo, c.Hi))
}

// Decode is the integer v, if it is in range.
func (c Uint64In) Decode(v Value) (uint64, error) { return decodeUint64In(v, c.Lo, c.Hi) }

// Uint32In is an integer in Lo..Hi, as a uint32.
type Uint32In struct{ Lo, Hi uint32 }

// Encode is x, if it is in range.
func (c Uint32In) Encode(x uint32, at Encoding) (Value, error) {
	return at.built(encodeUint32In(x, c.Lo, c.Hi))
}

// Decode is the integer v, if it is in range.
func (c Uint32In) Decode(v Value) (uint32, error) { return decodeUint32In(v, c.Lo, c.Hi) }

// Int64In is an integer in Lo..Hi, as an int64.
type Int64In struct{ Lo, Hi int64 }

// Encode is x, if it is in range.
func (c Int64In) Encode(x int64, at Encoding) (Value, error) {
	return at.built(encodeInt64In(x, c.Lo, c.Hi))
}

// Decode is the integer v, if it is in range.
func (c Int64In) Decode(v Value) (int64, error) { return decodeInt64In(v, c.Lo, c.Hi) }

// Int32In is an integer in Lo..Hi, as an int32.
type Int32In struct{ Lo, Hi int32 }

// Encode is x, if it is in range.
func (c Int32In) Encode(x int32, at Encoding) (Value, error) {
	return at.built(encodeInt32In(x, c.Lo, c.Hi))
}

// Decode is the integer v, if it is in range.
func (c Int32In) Decode(v Value) (int32, error) { return decodeInt32In(v, c.Lo, c.Hi) }

// IntIn is an integer in Lo..Hi, as an Int: a range wider than 64 bits.
type IntIn struct{ Lo, Hi BigBound }

// Encode is x, if it is in range.
func (c IntIn) Encode(x Int, at Encoding) (Value, error) { return at.built(encodeIntIn(x, c.Lo, c.Hi)) }

// Decode is the integer v, if it is in range.
func (c IntIn) Decode(v Value) (Int, error) { return decodeIntIn(v, c.Lo, c.Hi) }

// SizedBytes is a byte string of Lo to Hi bytes (Hi negative: unbounded).
type SizedBytes struct{ Lo, Hi int }

// Encode is a copy of b, if it is in size.
func (c SizedBytes) Encode(b []byte, at Encoding) (Value, error) {
	if len(b) < c.Lo || (c.Hi >= 0 && len(b) > c.Hi) {
		return encodeSizedBytes(b, c.Lo, c.Hi)
	}
	return EncodeBytes(b, at)
}

// Decode is a copy of the byte string v, if it is in size.
func (c SizedBytes) Decode(v Value) ([]byte, error) { return decodeSizedBytes(v, c.Lo, c.Hi) }

// SizedText is text whose UTF-8 encoding is Lo to Hi bytes long (Hi negative: unbounded).
type SizedText struct{ Lo, Hi int }

// Encode is the text s, if it is in size.
func (c SizedText) Encode(s string, at Encoding) (Value, error) {
	return at.text(s, func(s string) (Value, error) { return encodeSizedText(s, c.Lo, c.Hi) })
}

// Decode is the text v, if it is in size.
func (c SizedText) Decode(v Value) (string, error) { return decodeSizedText(v, c.Lo, c.Hi) }

// Matching is text matching Pattern (CDDL text .regexp).
type Matching struct{ Pattern TextPattern }

// Encode is the text s, if it matches.
func (c Matching) Encode(s string, at Encoding) (Value, error) {
	return at.text(s, func(s string) (Value, error) { return encodeMatching(s, c.Pattern) })
}

// Decode is the text v, if it matches.
func (c Matching) Decode(v Value) (string, error) { return decodeMatching(v, c.Pattern) }

// VecOf is a homogeneous array of Lo to Hi elements (Hi negative: unbounded), each read and written by Enc and Dec.
type VecOf[T any] struct {
	Lo, Hi int
	Enc    func(T, Encoding) (Value, error)
	Dec    func(Value) (T, error)
}

// Encode is the array of xs, one level down from at.
func (c VecOf[T]) Encode(xs []T, at Encoding) (Value, error) {
	if c.Enc == nil {
		return Value{}, noEncoder()
	}
	return encodeVec(xs, c.Lo, c.Hi, at, c.Enc)
}

// Decode is the elements of the array v.
func (c VecOf[T]) Decode(v Value) ([]T, error) {
	if c.Dec == nil {
		return nil, noDecoder()
	}
	return decodeVec(v, c.Lo, c.Hi, c.Dec)
}

// TextMapCodec is a map with arbitrary text keys and values of one type (CDDL { * text => T }), held as a TextMap.
type TextMapCodec[T any] struct {
	Enc func(T, Encoding) (Value, error)
	Dec func(Value) (T, error)
}

// Encode is the map of m, one level down from at, in canonical key order.
func (c TextMapCodec[T]) Encode(m TextMap[T], at Encoding) (Value, error) {
	if c.Enc == nil {
		return Value{}, noEncoder()
	}
	return encodeTextMap(m, at, c.Enc)
}

// Decode is the entries of the map v.
func (c TextMapCodec[T]) Decode(v Value) (TextMap[T], error) {
	if c.Dec == nil {
		return TextMap[T]{}, noDecoder()
	}
	return decodeTextMap(v, c.Dec)
}

// Pointer is a pointer to a value read and written by Enc and Dec: a field holding its own record by value (which Go
// cannot size) holds a pointer, and a nil one is a TypeMismatch.
type Pointer[T any] struct {
	Enc func(T, Encoding) (Value, error)
	Dec func(Value) (T, error)
}

// Encode is *p, at at.
func (c Pointer[T]) Encode(p *T, at Encoding) (Value, error) {
	if c.Enc == nil {
		return Value{}, noEncoder()
	}
	return encodePointer(p, at, c.Enc)
}

// Decode is a pointer to a fresh decoded value.
func (c Pointer[T]) Decode(v Value) (*T, error) {
	if c.Dec == nil {
		return nil, noDecoder()
	}
	return decodePointer(v, c.Dec)
}
