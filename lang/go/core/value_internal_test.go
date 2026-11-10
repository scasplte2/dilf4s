package core

import (
	"crypto/sha256"
	"errors"
	"fmt"
	"math"
	"math/big"
	"runtime"
	"strconv"
	"strings"
	"testing"
)

// doubled is [leaf, leaf] nested k deep: 2^k leaves in k arrays.
func doubled(t *testing.T, leaf Value, k int) Value {
	v := leaf
	for range k {
		v = must(t)(Array(v, v))
	}
	return v
}

// Equality never expands shared parts: two values of 2^40 leaves, built apart, compare in O(their distinct parts).
func TestEqualityCostsTheDistinctPairsNotTheWeight(t *testing.T) {
	big1 := must(t)(Text(strings.Repeat("x", 100)))
	big2 := must(t)(Text(strings.Repeat("x", 100)))
	a, b := doubled(t, big1, 40), doubled(t, big2, 40)
	var c comparison
	if !c.equal(a, b) {
		t.Fatal("equal values compare unequal")
	}
	if c.steps > 4*40 {
		t.Fatalf("%d steps for 2^40 leaves", c.steps)
	}
	different := doubled(t, must(t)(Text(strings.Repeat("y", 100))), 40)
	var d comparison
	if d.equal(a, different) || d.steps > 4*40 {
		t.Fatalf("unequal values: %d steps", d.steps)
	}
	// the same node is the same value at once
	var e comparison
	if !e.equal(a, a) || e.steps != 1 {
		t.Fatalf("a value with itself: %d steps", e.steps)
	}
	if !Null().Equal(Value{}) || Null().Equal(Bool(false)) || !Int64(5).Equal(Uint64(5)) {
		t.Fatal("scalars")
	}
}

// Every zero value is usable without a panic: the zero Value is null, and any other zero value stands for nothing and
// is refused where it is used.
func TestZeroValuesAreRefusedNotPanickedOn(t *testing.T) {
	var v Value
	if v.Kind() != KindNull || v.Weight() != 1 || v.Depth() != 0 || v.Len() != 0 || v.String() != "null" {
		t.Fatal("the zero Value is null")
	}
	if _, ok := v.Get("k"); ok {
		t.Fatal("null has no entries")
	}
	if _, ok := v.Index(0); ok {
		t.Fatal("null has no elements")
	}
	for range v.Entries() {
		t.Fatal("null has no entries")
	}
	if b, err := Encode(v); err != nil || string(b) != "\xf6" {
		t.Fatalf("null encodes as %x, %v", b, err)
	}
	var i Int
	if i.Sign() != 0 || i.String() != "0" || i.BitLength() != 0 || !i.Equal(IntOf(0)) {
		t.Fatal("the zero Int is 0")
	}
	var tag DomainTag
	if _, err := NewTaggedHash(SHA256, tag); !isHashKind(err, HashErrorKindInvalidDomainTag) {
		t.Errorf("the zero DomainTag: %v", err)
	}
	if _, err := TaggedHashOf(SHA256, tag, nil); !isHashKind(err, HashErrorKindInvalidDomainTag) {
		t.Errorf("the zero DomainTag: %v", err)
	}
	var th TaggedHash
	if _, err := th.Hash([]byte("m")); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("the zero TaggedHash: %v", err)
	}
	if _, err := th.Content(v); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("the zero TaggedHash: %v", err)
	}
	if _, err := th.Start(); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("the zero TaggedHash: %v", err)
	}
	for _, a := range []HashAlgorithm{"", "sha-256", HashAlgorithm(strings.Repeat("x", 1000))} {
		if _, err := Digest(a, nil); !isHashKind(err, HashErrorKindUnknownAlgorithm) {
			t.Errorf("algorithm %.10q: %v", a, err)
		}
		if _, err := a.OutputSize(); !isHashKind(err, HashErrorKindUnknownAlgorithm) {
			t.Errorf("algorithm %.10q: %v", a, err)
		}
		if _, err := ExpandMessageXmd(a, nil, []byte("d"), 1); !isHashKind(err, HashErrorKindUnknownAlgorithm) {
			t.Errorf("algorithm %.10q: %v", a, err)
		}
		if _, err := HashCommitment(a, nil, v); !isHashKind(err, HashErrorKindUnknownAlgorithm) {
			t.Errorf("algorithm %.10q: %v", a, err)
		}
	}
	if err := WriteValue(nil, v, 10); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("a nil hash: %v", err)
	}
	if err := WriteCommitment(nil, v, 10); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("a nil hash: %v", err)
	}
	var r Rational
	if r.Numerator().Sign() != 0 || r.Denominator().Sign() != 0 {
		t.Error("the zero Rational holds nothing")
	}
	var p Preview
	if p.Head() != "" || p.Length() != 0 || p.Truncated() {
		t.Error("the zero Preview is the preview of empty text")
	}
	var f DecodeFailure
	if f.Error() != "<root>: " {
		t.Errorf("the zero DecodeFailure: %q", f.Error())
	}
	var o Optional[Value]
	if o.IsSome() || o.String() != "none" {
		t.Error("the zero Optional is absent")
	}
	if _, err := (VecOf[uint64]{}).Encode(nil, Encoding{}); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("a codec without functions: %v", err)
	}
	if _, err := (Pointer[uint64]{Enc: EncodeUint}).Encode(nil, Encoding{}); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("a nil pointer: %v", err)
	}
	if _, ok := RationalOf(nil, big.NewInt(3)); ok {
		t.Error("a nil numerator")
	}
	if n, err := IntFromBig(nil); err != nil || n.Sign() != 0 {
		t.Error("a nil integer is 0")
	}
	if (TextPattern(0)).Matches("") || (TextPattern(9)).Source() != "<no pattern>" {
		t.Error("a value that is no pattern matches nothing")
	}
}

func isHashKind(err error, kind HashErrorKind) bool {
	var he HashError
	return errors.As(err, &he) && he.Kind() == kind
}

// Nothing is shared with a caller: what is passed in is copied before it is kept, and what is handed out is a copy.
func TestNothingIsSharedWithTheCaller(t *testing.T) {
	b := []byte{1, 2, 3}
	v := Bytes(b)
	b[0] = 9
	out, _ := v.AsBytes()
	if out[0] != 1 {
		t.Fatal("Bytes kept the caller's slice")
	}
	out[1] = 9
	if again, _ := v.AsBytes(); again[1] != 2 {
		t.Fatal("AsBytes handed out the value's own bytes")
	}
	n := big.NewInt(42)
	i, _ := IntFromBig(n)
	n.SetInt64(7)
	if i.String() != "42" {
		t.Fatal("IntFromBig kept the caller's integer")
	}
	i.Big().SetInt64(7)
	if i.String() != "42" {
		t.Fatal("Big handed out the integer itself")
	}
	r, _ := must(t)(Number(big.NewInt(1), big.NewInt(3))).AsRational()
	r.Numerator().SetInt64(5)
	r.Denominator().SetInt64(7)
	if r.Numerator().Int64() != 1 || r.Denominator().Int64() != 3 {
		t.Fatal("a rational handed out its parts")
	}
	items := []Value{Int64(1), Int64(2)}
	a := must(t)(Array(items...))
	items[0] = Int64(9)
	got := a.Items()
	if first, _ := a.Index(0); !first.Equal(Int64(1)) {
		t.Fatal("Array kept the caller's slice")
	}
	got[1] = Int64(9)
	if second, _ := a.Index(1); !second.Equal(Int64(2)) {
		t.Fatal("Items handed out the array's own slice")
	}
	m := must(t)(Map(Entry{Key: "a", Value: Int64(1)}))
	keys := m.Keys()
	keys[0] = "z"
	if _, ok := m.Get("a"); !ok {
		t.Fatal("Keys handed out the map's own slice")
	}
	dst := []byte("dst")
	expanded, _ := ExpandMessageXmd(SHA256, [][]byte{[]byte("m")}, dst, 32)
	again, _ := ExpandMessageXmd(SHA256, [][]byte{[]byte("m")}, []byte("dst"), 32)
	if string(expanded) != string(again) {
		t.Fatal("expand_message_xmd differs")
	}
	digest := make([]byte, 32)
	d, _ := DigestFromBytes(SHA256, digest)
	digest[0] = 1
	if d[0] != 0 {
		t.Fatal("DigestFromBytes kept the caller's slice")
	}
}

// A map drops null values, refuses a repeated key (even with a null value) at its first repeat with a preview, and
// orders its keys canonically.
func TestMapConstruction(t *testing.T) {
	m := must(t)(Map(Entry{Key: "bb", Value: Int64(1)}, Entry{Key: "a", Value: Null()}, Entry{Key: "c", Value: Int64(3)}))
	if m.Len() != 2 || strings.Join(m.Keys(), ",") != "c,bb" {
		t.Fatalf("keys %v", m.Keys())
	}
	if m.Weight() != 1+(1+1+1)+(1+2+1) {
		t.Fatalf("weight %d", m.Weight())
	}
	var dup CanonicalErrorDuplicateKey
	long := strings.Repeat("k", 100)
	if _, err := Map(Entry{Key: long, Value: Null()}, Entry{Key: "x", Value: Int64(1)}, Entry{Key: long, Value: Int64(2)}); !errors.As(err, &dup) {
		t.Fatalf("a repeat: %v", err)
	}
	if dup.Key.Length() != 100 || !dup.Key.Truncated() || len([]rune(dup.Key.Head())) != MaxPreviewCodePoints {
		t.Fatalf("the preview %v", dup.Key)
	}
	deep := Null()
	for range MaxDepth {
		deep = must(t)(Array(deep))
	}
	if _, err := Map(Entry{Key: "k", Value: deep}); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Fatalf("past the depth: %v", err)
	}
}

// A recipe's work is bounded however many times its steps repeat, and copies are bounded.
func TestBuildingIsBoundedWhateverTheRecipe(t *testing.T) {
	null, _ := Encode(Null())
	dollar, _ := Encode(must(t)(Text("$")))
	pair, _ := Encode(must(t)(Array(must(t)(Text("$")), must(t)(Text("$")))))
	inMap, _ := Encode(must(t)(Map(Entry{Key: "k", Value: must(t)(Text("$"))})))
	constant, _ := Encode(Int64(7))
	if v, err := Build(null, []BuildStep{{Template: pair, Times: 40}}); err != nil || v.Weight() != 1<<41-1 {
		t.Fatalf("doubling: %v", err)
	}
	// a null placed in a map is dropped once; the map placed next is not, so the depth limit ends it
	if v, err := Build(null, []BuildStep{{Template: inMap, Times: 1<<64 - 1}}); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Fatalf("null wrapped in a map: %v, %v", v, err)
	}
	if v, err := Build(constant, []BuildStep{{Template: dollar, Times: 1<<64 - 1}}); err != nil || !v.Equal(Int64(7)) {
		t.Fatalf("the template $: %v", err)
	}
	if v, err := Build(null, []BuildStep{{Template: constant, Times: 1<<64 - 1}}); err != nil || !v.Equal(Int64(7)) {
		t.Fatalf("a template without $: %v", err)
	}
	if _, err := Build(null, []BuildStep{{Template: pair, Times: 1<<64 - 1}}); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Fatalf("an unbounded wrap: %v", err)
	}
	if _, err := Build(null, []BuildStep{{Copies: Some(uint64(MaxCopies + 1))}}); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Fatalf("too many copies: %v", err)
	}
	if v, err := Build(constant, []BuildStep{{Template: inMap, Times: 1}, {Copies: Some(uint64(3))}}); err != nil || v.Len() != 3 {
		t.Fatalf("a wrap then copies: %v", err)
	}
}

// A diagnostic rendering is bounded however heavy the value (a few bytes per unit of DebugBudget), and shows a wide
// number by its bit length.
func TestStringIsBounded(t *testing.T) {
	v := doubled(t, must(t)(Text(strings.Repeat("é", 50))), 60)
	s := v.String()
	if len(s) > 16*DebugBudget || !strings.Contains(s, "…") {
		t.Fatalf("%d bytes", len(s))
	}
	wide := must(t)(Integer(new(big.Int).Lsh(big.NewInt(1), 10_000)))
	if s := wide.String(); strings.Contains(s, "1995") || !strings.Contains(s, "10001 bits") {
		t.Fatalf("a wide integer shows as %.60s", s)
	}
}

// The hash functions are the ones named: each streams what it is given, a part at a time.
func TestStreamingMatchesTheBuiltEncoding(t *testing.T) {
	v := must(t)(ParseJSON(`{"a":[1,2,{"b":"` + strings.Repeat("z", 5000) + `"}],"n":-18446744073709551617}`))
	b, _ := Encode(v)
	tag, _ := NewDomainTag("dilf4s/test/v1")
	th, _ := NewTaggedHash(SHA256, tag)
	built, _ := th.Hash(b)
	streamed, err := th.Content(v)
	if err != nil || string(built) != string(streamed) {
		t.Fatalf("streamed %x, built %x, %v", streamed, built, err)
	}
	h := sha256.New()
	if err := WriteValue(h, v, MaxEncodedBytes); err != nil {
		t.Fatal(err)
	}
	direct := sha256.Sum256(b)
	if string(h.Sum(nil)) != string(direct[:]) {
		t.Fatal("WriteValue differs from hashing the encoding")
	}
	if _, err := th.ContentWithin(v, len(b)-1); !errors.As(err, new(TooLarge)) {
		t.Fatalf("within one byte under: %v", err)
	}
	commitment, _ := ValueCommitment(v)
	h.Reset()
	if err := WriteCommitment(h, v, len(commitment)); err != nil {
		t.Fatal(err)
	}
	if want := sha256.Sum256(commitment); string(h.Sum(nil)) != string(want[:]) {
		t.Fatal("WriteCommitment differs from hashing the commitment")
	}
	if _, err := ValueCommitmentWithin(v, len(commitment)-1); !errors.As(err, new(TooLarge)) {
		t.Fatalf("a commitment within one byte under: %v", err)
	}
	if _, err := ValueCommitmentWithin(Bytes([]byte{1, 2}), 2); !errors.As(err, new(TooLarge)) {
		t.Fatalf("a byte string's commitment is one byte longer: %v", err)
	}
}

// An array's length is checked against the budget before anything is allocated: a slice of a zero-size type may be
// any length in no memory, and its slots are never made past the budget.
func TestAnArrayIsRefusedByItsLengthBeforeItsSlots(t *testing.T) {
	empty := func(struct{}, Encoding) (Value, error) { return Bool(true), nil }
	vec := VecOf[struct{}]{Lo: 0, Hi: -1, Enc: empty}
	for _, at := range []Encoding{EncodingWithin(1), EncodingWithin(1000), {}} {
		var tooLarge TooLarge
		if _, err := vec.Encode(make([]struct{}, math.MaxInt), at); !errors.As(err, &tooLarge) {
			t.Errorf("within %d: %v", at.budget(), err)
		}
	}
	if v, err := vec.Encode(make([]struct{}, 999), EncodingWithin(1000)); err != nil || v.Len() != 999 {
		t.Errorf("an array within its budget: %v", err)
	}
	if _, err := vec.Encode(make([]struct{}, 1000), EncodingWithin(1000)); !errors.As(err, new(TooLarge)) {
		t.Errorf("one element past its budget: %v", err)
	}
}

// allocatedBy is the bytes f allocates.
func allocatedBy(f func()) uint64 {
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	f()
	runtime.ReadMemStats(&after)
	return after.TotalAlloc - before.TotalAlloc
}

// refusedAtTheFirst checks that an array and a map of n nulls, decoded as T (which no null is), are refused at their
// first element having allocated at most bound bytes, never panicking.
func refusedAtTheFirst[T any](t *testing.T, n int, bound uint64) {
	t.Helper()
	refuse := func(v Value) (T, error) {
		var zero T
		return zero, expected("no null", v)
	}
	nulls := must(t)(arrayOf(make([]Value, n)))
	keys := make([]string, n)
	for i := range keys {
		keys[i] = fmt.Sprintf("%05d", i)
	}
	nullMap := must(t)(mapOfSorted(keys, make([]Value, n)))
	var vecErr, mapErr error
	if used := allocatedBy(func() { _, vecErr = (VecOf[T]{Lo: 0, Hi: -1, Dec: refuse}).Decode(nulls) }); used > bound {
		t.Errorf("an array refused at its first element allocated %d bytes", used)
	}
	if used := allocatedBy(func() { _, mapErr = (TextMapCodec[T]{Dec: refuse}).Decode(nullMap) }); used > bound {
		t.Errorf("a map refused at its first value allocated %d bytes", used)
	}
	var failure DecodeFailure
	if !errors.As(vecErr, &failure) || !errors.As(mapErr, &failure) {
		t.Errorf("refused: %v, %v", vecErr, mapErr)
	}
}

// A decoded container grows from nothing as its elements decode: an element type far larger than the value it decodes
// from (a fixed byte array from a null) never has a slot made ahead of a refusal. 65,536 of 64 KiB, or 64 of 64 MiB,
// would be 2^32 bytes, which a 32-bit make cannot even ask for. The 64 MiB elements are tried on a 64-bit int only:
// Go returns a value on the stack, and a few of them pass a 32-bit goroutine's 250 MB stack in any function returning
// one. Each such element tried is 64 MiB, so that bound is wider.
func TestADecodedContainerGrowsAsItsElementsDecode(t *testing.T) {
	refusedAtTheFirst[[1 << 16]byte](t, 1<<16, 16<<20)
	if strconv.IntSize == 64 {
		refusedAtTheFirst[[1 << 26]byte](t, 64, 1<<30)
	}
	type wide = [1 << 16]byte
	dec := func(v Value) (wide, error) {
		var out wide
		b, ok := v.AsBytes()
		if !ok || len(b) != len(out) {
			return out, expected("64 KiB of bytes", v)
		}
		copy(out[:], b)
		return out, nil
	}
	// what does decode is all there
	two := must(t)(Array(Bytes(make([]byte, 1<<16)), Bytes(append(make([]byte, 1<<16-1), 7))))
	if got, err := (VecOf[wide]{Lo: 0, Hi: -1, Dec: dec}).Decode(two); err != nil || len(got) != 2 || got[1][1<<16-1] != 7 {
		t.Errorf("decoded: %d elements, %v", len(got), err)
	}
	if got, err := (VecOf[wide]{Lo: 0, Hi: -1, Dec: dec}).Decode(must(t)(Array())); err != nil || got == nil || len(got) != 0 {
		t.Errorf("an empty array: %v, %v", got == nil, err)
	}
}

// matchesStateKey is the state-key grammar written out the obvious way, splitting the key into its segments.
func matchesStateKey(text string) bool {
	for _, segment := range strings.Split(text, "/") {
		if segment == "" {
			return false
		}
		for i := 0; i < len(segment); i++ {
			c := segment[i]
			lowerDigit := (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
			if !lowerDigit && (i == 0 || (c != '.' && c != '_' && c != '-')) {
				return false
			}
		}
	}
	return true
}

// A state key is matched in one pass that allocates nothing, agreeing with the grammar on every key of up to six
// bytes over an alphabet of each kind of byte.
func TestAStateKeyIsMatchedInOnePass(t *testing.T) {
	alphabet := []byte("a0._-/A\xc3")
	keys := []string{""}
	for frontier := []string{""}; len(frontier[0]) < 6; {
		var next []string
		for _, k := range frontier {
			for _, c := range alphabet {
				next = append(next, k+string(c))
			}
		}
		keys, frontier = append(keys, next...), next
	}
	for _, k := range keys {
		if got, want := PatternStateKey.Matches(k), matchesStateKey(k); got != want {
			t.Errorf("%q: matched %v, the grammar says %v", k, got, want)
		}
	}
	long := strings.Repeat("a/", 1<<16) + "a"
	if allocs := testing.AllocsPerRun(10, func() { PatternStateKey.Matches(long) }); allocs != 0 {
		t.Errorf("matching allocated %v times", allocs)
	}
}

// A caller's text is refused by its length against the budget before it is validated or matched.
func TestATextIsRefusedByItsLengthBeforeItIsChecked(t *testing.T) {
	key := strings.Repeat("a/", 8<<20) + "a"
	encoders := map[string]func(string, Encoding) (Value, error){
		"text":      EncodeText,
		"sized":     SizedText{Lo: 0, Hi: -1}.Encode,
		"state key": Matching{Pattern: PatternStateKey}.Encode,
	}
	for name, enc := range encoders {
		var err error
		if used := allocatedBy(func() { _, err = enc(key, EncodingWithin(1)) }); used > 1<<20 {
			t.Errorf("%s: refused after allocating %d bytes", name, used)
		}
		if !errors.As(err, new(TooLarge)) {
			t.Errorf("%s: %v", name, err)
		}
		if v, err := enc(key, EncodingWithin(uint64(len(key))+1)); err != nil || v.Weight() != uint64(len(key))+1 {
			t.Errorf("%s within its budget: %v", name, err)
		}
	}
}

// An encoding is charged what its value weighs: a map's null member, which the value leaves out, is charged apart with
// its key, so a model spelling a null is refused at a limit exactly when one leaving it out is. An array keeps its null
// elements, and is charged for them.
func TestAnEncodingIsChargedWhatItsValueWeighs(t *testing.T) {
	record := func(withNull bool) func(struct{}, Encoding) (Value, error) {
		return func(_ struct{}, at Encoding) (Value, error) {
			inner, err := at.Nested()
			if err != nil {
				return Value{}, err
			}
			x, err := EncodeText("x", inner)
			if err != nil {
				return Value{}, err
			}
			null, err := EncodeAny(Null(), inner)
			if err != nil {
				return Value{}, err
			}
			return inner.Record(Req("var", x), Opt("default", Optional[Value]{value: null, ok: withNull}))
		}
	}
	for _, withNull := range []bool{false, true} {
		got, err := ToCBORWithin(struct{}{}, record(withNull), 7)
		if fmt.Sprintf("%x", got) != "a1637661726178" || err != nil {
			t.Errorf("with a null %v, within 7 bytes: %x, %v", withNull, got, err)
		}
		if _, err := ToCBORWithin(struct{}{}, record(withNull), 6); !errors.As(err, new(TooLarge)) {
			t.Errorf("with a null %v, within 6 bytes: %v", withNull, err)
		}
	}
	m, err := NewTextMap(TextEntry[Value]{Key: "a", Value: Null()}, TextEntry[Value]{Key: "b", Value: must(t)(Text("c"))})
	if err != nil {
		t.Fatal(err)
	}
	maps := TextMapCodec[Value]{Enc: EncodeAny}
	if v, err := maps.Encode(m, EncodingWithin(5)); err != nil || v.Weight() != 5 {
		t.Errorf("a map dropping a null, within its weight: %v", err)
	}
	if _, err := maps.Encode(m, EncodingWithin(4)); !errors.As(err, new(TooLarge)) {
		t.Errorf("a map dropping a null, past its weight: %v", err)
	}
	nulls := VecOf[Value]{Lo: 0, Hi: -1, Enc: EncodeAny}
	if v, err := nulls.Encode([]Value{Null(), Null()}, EncodingWithin(3)); err != nil || v.Weight() != 3 {
		t.Errorf("an array keeping two nulls, within its weight: %v", err)
	}
	if _, err := nulls.Encode([]Value{Null(), Null()}, EncodingWithin(2)); !errors.As(err, new(TooLarge)) {
		t.Errorf("an array keeping two nulls, past its weight: %v", err)
	}
}

// What a map drops is bounded apart, by DefaultEncodingWeight or the budget if larger: a model sharing a map of nulls
// is refused once the nulls dropped (with their keys) weigh more, though the value it stands for is an array of empty
// maps. A map of 1024 nulls under four-byte keys drops 1024 * (5 + 1) = 6144; 2730 of them drop 16,773,120, within
// 2^24, and 2731 drop 16,779,264, past it.
func TestWhatAMapDropsIsBoundedApart(t *testing.T) {
	entries := make([]TextEntry[Value], 1024)
	for i := range entries {
		entries[i] = TextEntry[Value]{Key: fmt.Sprintf("%04d", i), Value: Null()}
	}
	m, err := NewTextMap(entries...)
	if err != nil {
		t.Fatal(err)
	}
	shared := func(k int) []TextMap[Value] {
		out := make([]TextMap[Value], k)
		for i := range out {
			out[i] = m
		}
		return out
	}
	vec := VecOf[TextMap[Value]]{Lo: 0, Hi: -1, Enc: TextMapCodec[Value]{Enc: EncodeAny}.Encode}
	if v, err := vec.Encode(shared(2730), Encoding{}); err != nil || v.Weight() != 1+2730 {
		t.Errorf("2730 maps of nulls: %v", err)
	}
	var tooLarge TooLarge
	if _, err := vec.Encode(shared(2731), Encoding{}); !errors.As(err, &tooLarge) || tooLarge.Limit != DefaultEncodingWeight {
		t.Errorf("2731 maps of nulls: %v", err)
	}
	if _, err := vec.Encode(shared(2731), EncodingWithin(2*DefaultEncodingWeight)); err != nil {
		t.Errorf("2731 maps of nulls, within a larger budget: %v", err)
	}
	// ToCBORWithin refuses with the allowance as the limit, not the byte limit the value is well within
	if b, err := ToCBORWithin(shared(2730), vec.Encode, 4000); err != nil || len(b) != 3+2730 {
		t.Errorf("2730 maps of nulls within 4000 bytes: %d bytes, %v", len(b), err)
	}
	if _, err := ToCBORWithin(shared(2731), vec.Encode, 4000); !errors.As(err, &tooLarge) || tooLarge.Limit != DefaultEncodingWeight {
		t.Errorf("2731 maps of nulls within 4000 bytes: %v", err)
	}
}

// A map's key is charged by its length before it is checked as text, as a text is: a key past the budget is TooLarge
// however ill-formed (a kept key against the budget, a dropped one against the allowance), and one within it is
// InvalidUtf8 at its first ill-formed byte.
func TestAKeyIsRefusedByItsLengthBeforeItIsChecked(t *testing.T) {
	keyed := func(key string, value Value) TextMap[Value] {
		m, err := NewTextMap(TextEntry[Value]{Key: key, Value: value})
		if err != nil {
			t.Fatal(err)
		}
		return m
	}
	maps := TextMapCodec[Value]{Enc: EncodeAny}
	kept := keyed(strings.Repeat("a", 1<<20)+"\xff", must(t)(Text("v")))
	if _, err := maps.Encode(kept, EncodingWithin(1)); !errors.As(err, new(TooLarge)) {
		t.Errorf("a kept key past the budget: %v", err)
	}
	var invalid CanonicalErrorInvalidUtf8
	if _, err := maps.Encode(kept, Encoding{}); !errors.As(err, &invalid) || invalid.Offset != 1<<20 {
		t.Errorf("a kept key within the budget: %v", err)
	}
	dropped := keyed(strings.Repeat("a", DefaultEncodingWeight)+"\xff", Null())
	if _, err := maps.Encode(dropped, EncodingWithin(1)); !errors.As(err, new(TooLarge)) {
		t.Errorf("a dropped key past the allowance: %v", err)
	}
	if _, err := maps.Encode(dropped, EncodingWithin(2*DefaultEncodingWeight)); !errors.As(err, &invalid) || invalid.Offset != DefaultEncodingWeight {
		t.Errorf("a dropped key within the allowance: %v", err)
	}
}
