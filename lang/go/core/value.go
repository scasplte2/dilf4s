package core

import (
	"iter"
	"math/big"
	"slices"
	"sort"
	"unicode/utf8"
)

// MaxDepth is the nesting limit for arrays and maps: a scalar has depth 0, an array or map one more than its deepest
// element (1 when empty).
const MaxDepth = CanonicalMaxDepth

// MaxWeight is the greatest Value.Weight: weights saturate here (the reference's Long.MaxValue), never wrap.
const MaxWeight uint64 = CanonicalMaxWeight

// Kind is the kind of a canonical value.
type Kind uint8

// The kinds of canonical values (docs/specs/canonical-encoding.md section 1).
const (
	KindNull Kind = iota
	KindBool
	KindInt
	KindRational
	KindText
	KindBytes
	KindArray
	KindMap
)

// String is the kind's name, as messages use it.
func (k Kind) String() string {
	switch k {
	case KindNull:
		return "null"
	case KindBool:
		return "bool"
	case KindInt:
		return "int"
	case KindRational:
		return "rational"
	case KindText:
		return "text"
	case KindBytes:
		return "bytes"
	case KindArray:
		return "array"
	case KindMap:
		return "map"
	}
	return "unknown"
}

// Value is a canonical value: null, a boolean, an integer, a rational, text, bytes, an array or a map with text keys.
// It is an immutable handle, cheap to copy; the zero Value is null. Every invariant of the data model holds by
// construction: integers within MaxIntegerBits, rationals reduced and never integers, text well-formed UTF-8, map
// keys unique and in canonical order with no null values, nesting within MaxDepth. So every Value has exactly one
// encoding and decodes back to itself.
//
// Values share structure: a value may occur many times inside another without being copied, and its weight (the
// logical, unshared size) and depth are cached when it is built, so reading either is O(1). Anything that expands a
// value (encoding, rendering, hashing, typed decoding) costs O(weight): bound the weight of a value not known to be
// small first, as the budgeted forms (EncodeWithin, RenderWithin, DecodeWithin) do.
//
// A Value must never hold secret material: its memory is shared and cannot be reliably zeroized.
type Value struct {
	n *node
}

// node is a built value, never changed after construction and never handed out.
type node struct {
	kind   Kind
	b      bool
	num    Int // an integer, or a rational's numerator
	den    Int // a rational's denominator
	text   string
	bytes  []byte
	items  []Value
	keys   []string // a map's keys, in canonical key order
	vals   []Value
	depth  int
	weight uint64
}

// addWeight is a + b, saturating at MaxWeight.
func addWeight(a, b uint64) uint64 {
	s := a + b
	if s < a || s > MaxWeight {
		return MaxWeight
	}
	return s
}

// sizedWeight is one node plus n units (bytes of text or of a byte string).
func sizedWeight(n int) uint64 { return addWeight(1, uint64(n)) }

// Null is the null value, as is the zero Value.
func Null() Value { return Value{} }

// Bool is the boolean b.
func Bool(b bool) Value { return Value{&node{kind: KindBool, b: b, weight: 1}} }

// IntValue is the integer i: every Int is within MaxIntegerBits.
func IntValue(i Int) Value { return Value{&node{kind: KindInt, num: i, weight: i.weight()}} }

// Int64 is the integer n.
func Int64(n int64) Value { return IntValue(IntOf(n)) }

// Uint64 is the integer n.
func Uint64(n uint64) Value { return IntValue(UintOf(n)) }

// Integer is the integer n (copied), or CanonicalErrorIntegerTooWide when it is wider than MaxIntegerBits.
func Integer(n *big.Int) (Value, error) {
	i, err := IntFromBig(n)
	if err != nil {
		return Value{}, err
	}
	return IntValue(i), nil
}

// Text is the text s, or CanonicalErrorInvalidUtf8 at the first byte of s that is not well-formed UTF-8 (the data
// model's text is Unicode scalar values; a Go string may hold any bytes).
func Text(s string) (Value, error) {
	if at, ok := invalidUTF8(s); ok {
		return Value{}, CanonicalErrorInvalidUtf8{Offset: uint64(at)}
	}
	return textOf(s), nil
}

// textOf is the text s, which is well-formed UTF-8.
func textOf(s string) Value {
	return Value{&node{kind: KindText, text: s, weight: sizedWeight(len(s))}}
}

// invalidUTF8 is the offset of the first byte of s that does not start a well-formed UTF-8 sequence, if any.
func invalidUTF8(s string) (int, bool) {
	if utf8.ValidString(s) {
		return 0, false
	}
	for i := 0; i < len(s); {
		r, size := utf8.DecodeRuneInString(s[i:])
		if r == utf8.RuneError && size <= 1 {
			return i, true
		}
		i += size
	}
	return len(s), true
}

// Bytes is the byte string holding a copy of b.
func Bytes(b []byte) Value { return bytesOf(slices.Clone(b)) }

// bytesOf is the byte string b, which this package made and gives to no one.
func bytesOf(b []byte) Value {
	if b == nil {
		b = []byte{}
	}
	return Value{&node{kind: KindBytes, bytes: b, weight: sizedWeight(len(b))}}
}

// Array is the array of items (the slice is copied), or CanonicalErrorDepthExceeded past MaxDepth.
func Array(items ...Value) (Value, error) { return arrayOf(slices.Clone(items)) }

// arrayOf is the array of items, which this package made and gives to no one.
func arrayOf(items []Value) (Value, error) {
	depth, weight := 0, uint64(1)
	for _, v := range items {
		depth = max(depth, v.Depth())
		weight = addWeight(weight, v.Weight())
	}
	if depth+1 > MaxDepth {
		return Value{}, depthExceeded()
	}
	if items == nil {
		items = []Value{}
	}
	return Value{&node{kind: KindArray, items: items, depth: depth + 1, weight: weight}}, nil
}

func depthExceeded() error { return CanonicalErrorDepthExceeded{Limit: MaxDepth} }

// Entry is a map entry.
type Entry struct {
	Key   string
	Value Value
}

// Map is the map of entries, in any order: null values are dropped; a repeated key (even with a null value) is
// CanonicalErrorDuplicateKey, a key that is not well-formed UTF-8 CanonicalErrorInvalidUtf8, each refused at the first
// such entry in the order given; nesting past MaxDepth is CanonicalErrorDepthExceeded.
func Map(entries ...Entry) (Value, error) {
	// the keys are the caller's: compared, never hashed
	var seen keySet
	kept := make([]Entry, 0, len(entries))
	for _, e := range entries {
		if at, ok := invalidUTF8(e.Key); ok {
			return Value{}, CanonicalErrorInvalidUtf8{Offset: uint64(at)}
		}
		if !seen.add(e.Key) {
			return Value{}, CanonicalErrorDuplicateKey{Key: PreviewOf(e.Key)}
		}
		if !e.Value.IsNull() {
			kept = append(kept, e)
		}
	}
	sort.SliceStable(kept, func(a, b int) bool { return canonicalKeyOrder(kept[a].Key, kept[b].Key) < 0 })
	keys := make([]string, len(kept))
	vals := make([]Value, len(kept))
	for i, e := range kept {
		keys[i], vals[i] = e.Key, e.Value
	}
	return mapOfSorted(keys, vals)
}

// mapOfSorted is the map of keys (unique, well-formed and in canonical order) to vals (none null), which this package
// made and gives to no one.
func mapOfSorted(keys []string, vals []Value) (Value, error) {
	depth, weight := 0, uint64(1)
	for i, v := range vals {
		depth = max(depth, v.Depth())
		weight = addWeight(weight, addWeight(sizedWeight(len(keys[i])), v.Weight()))
	}
	if depth+1 > MaxDepth {
		return Value{}, depthExceeded()
	}
	if keys == nil {
		keys, vals = []string{}, []Value{}
	}
	return Value{&node{kind: KindMap, keys: keys, vals: vals, depth: depth + 1, weight: weight}}, nil
}

// canonicalKeyOrder is the order of keys in an encoded map: their encodings compared bytewise (RFC 8949 section
// 4.2.1), which for text keys is shorter UTF-8 first, then bytewise.
func canonicalKeyOrder(a, b string) int {
	if len(a) != len(b) {
		if len(a) < len(b) {
			return -1
		}
		return 1
	}
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	}
	return 0
}

// Kind is the value's kind.
func (v Value) Kind() Kind {
	if v.n == nil {
		return KindNull
	}
	return v.n.kind
}

// IsNull reports whether v is null.
func (v Value) IsNull() bool { return v.Kind() == KindNull }

// Depth is the nesting depth: 0 for scalars, one more than the deepest element for arrays and maps (1 when empty).
// O(1): cached.
func (v Value) Depth() int {
	if v.n == nil {
		return 0
	}
	return v.n.depth
}

// Weight is a portable measure of the value's logical size, as the reference's CanonicalValue.weight: one per node,
// plus the UTF-8 length of each text and map key, the length of each byte string, and the whole 64-bit limbs of each
// number (bitLength / 64, numerator and denominator). A shared part counts every time it occurs, so the weight bounds
// the work of any full traversal; it saturates at MaxWeight. O(1): cached.
func (v Value) Weight() uint64 {
	if v.n == nil {
		return 1
	}
	return v.n.weight
}

// AsBool is the boolean, if v is one.
func (v Value) AsBool() (bool, bool) {
	if v.Kind() != KindBool {
		return false, false
	}
	return v.n.b, true
}

// AsInt is the integer, if v is one.
func (v Value) AsInt() (Int, bool) {
	if v.Kind() != KindInt {
		return Int{}, false
	}
	return v.n.num, true
}

// AsRational is the rational, if v is one.
func (v Value) AsRational() (Rational, bool) {
	if v.Kind() != KindRational {
		return Rational{}, false
	}
	return Rational{num: v.n.num, den: v.n.den}, true
}

// AsText is the text, if v is text.
func (v Value) AsText() (string, bool) {
	if v.Kind() != KindText {
		return "", false
	}
	return v.n.text, true
}

// AsBytes is a copy of the byte string, if v is one.
func (v Value) AsBytes() ([]byte, bool) {
	if v.Kind() != KindBytes {
		return nil, false
	}
	return slices.Clone(v.n.bytes), true
}

// Len is the number of elements of an array, entries of a map, or bytes of a byte string or text; 0 otherwise.
func (v Value) Len() int {
	switch v.Kind() {
	case KindArray:
		return len(v.n.items)
	case KindMap:
		return len(v.n.keys)
	case KindBytes:
		return len(v.n.bytes)
	case KindText:
		return len(v.n.text)
	}
	return 0
}

// Index is the array element at i, if v is an array with one there.
func (v Value) Index(i int) (Value, bool) {
	if v.Kind() != KindArray || i < 0 || i >= len(v.n.items) {
		return Value{}, false
	}
	return v.n.items[i], true
}

// Items is a copy of an array's elements (the values themselves are shared, immutable handles), or nil.
func (v Value) Items() []Value {
	if v.Kind() != KindArray {
		return nil
	}
	return slices.Clone(v.n.items)
}

// Get is the value at key, if v is a map with key present: O(log n), by binary search in canonical key order.
func (v Value) Get(key string) (Value, bool) {
	if v.Kind() != KindMap {
		return Value{}, false
	}
	i, found := slices.BinarySearchFunc(v.n.keys, key, canonicalKeyOrder)
	if !found {
		return Value{}, false
	}
	return v.n.vals[i], true
}

// Keys is a copy of a map's keys in canonical key order (shorter UTF-8 first, then bytewise), or nil.
func (v Value) Keys() []string {
	if v.Kind() != KindMap {
		return nil
	}
	return slices.Clone(v.n.keys)
}

// Entries yields a map's entries in canonical key order; nothing for any other value.
func (v Value) Entries() iter.Seq2[string, Value] {
	return func(yield func(string, Value) bool) {
		if v.Kind() != KindMap {
			return
		}
		for i, k := range v.n.keys {
			if !yield(k, v.n.vals[i]) {
				return
			}
		}
	}
}
