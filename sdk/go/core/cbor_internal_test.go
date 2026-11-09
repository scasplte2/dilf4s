package core

import (
	"bytes"
	"errors"
	"math"
	"math/big"
	"strconv"
	"testing"
)

// must is the value of a constructor's result, failing the test on its error.
func must(t testing.TB) func(Value, error) Value {
	return func(v Value, err error) Value {
		t.Helper()
		if err != nil {
			t.Fatal(err)
		}
		return v
	}
}

func encoded(t testing.TB, v Value) []byte {
	t.Helper()
	b, err := Encode(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// fibonacci is the consecutive Fibonacci numbers (F(k + 1), F(k)) with F(k + 1) exactly bits wide: coprime, and the
// pair whose gcd takes the most steps for its width.
func fibonacci(bits uint64) (*big.Int, *big.Int) {
	a, b := big.NewInt(1), big.NewInt(1)
	for uint64(b.BitLen()) < bits {
		a, b = b, new(big.Int).Add(a, b)
	}
	return b, a
}

// integerBytes is the canonical CBOR of the integer n, whatever its width: for a bignum too wide to be a value, the
// bytes a decoder must refuse.
func integerBytes(n *big.Int) []byte {
	// a budget no integer of the tests' reaches: the write always fits
	out := &budget{max: 1 << 30, writing: true}
	writeInteger(out, Int{v: n, bits: bitLength(n)})
	return out.out
}

func rationalBytes(n, d *big.Int) []byte {
	return append(append([]byte{0xd8, 0x1e, 0x82}, integerBytes(n)...), integerBytes(d)...)
}

// decoded is a decode through a reducer that counts its calls (each one gcd) and the widest part it was given, and a
// magnitude builder that counts its calls and the widest magnitude it built, in bytes.
type decoded struct {
	v               Value
	err             error
	gcds            int
	widest          uint64
	magnitudes      int
	widestMagnitude int
}

func counted(b []byte) decoded {
	var d decoded
	reduce := func(n, den *big.Int) (Value, bool) {
		d.gcds++
		d.widest = max(d.widest, bitLength(n), bitLength(den))
		return exactRational(n, den)
	}
	magnitude := func(b []byte) *big.Int {
		d.magnitudes++
		d.widestMagnitude = max(d.widestMagnitude, len(b))
		return magnitudeOf(b)
	}
	d.v, d.err = decodeWith(b, MaxDepth, reduce, magnitude)
	return d
}

func isKind(err error, kind CanonicalErrorKind) bool {
	var ce CanonicalError
	return errors.As(err, &ce) && ce.Kind() == kind
}

// Decoding k maximal rationals runs exactly k gcds, each of parts within the bound: linear in the input (each takes
// over 8 KiB). A wider part is refused before any gcd.
func TestDecodingARationalRunsOneGcdOfBoundedParts(t *testing.T) {
	n, d := fibonacci(MaxRationalBits)
	if bitLength(n) != MaxRationalBits || bitLength(d) != MaxRationalBits-1 {
		t.Fatalf("fibonacci: %d and %d bits", bitLength(n), bitLength(d))
	}
	r := must(t)(Number(n, d))
	const k = 8
	items := make([]Value, k)
	for i := range items {
		items[i] = r
	}
	maximal := must(t)(Array(items...))
	b := encoded(t, maximal)
	got := counted(b)
	if got.err != nil || !got.v.Equal(maximal) {
		t.Fatalf("maximal rationals decode: %v", got.err)
	}
	if got.gcds != k || got.widest != MaxRationalBits {
		t.Fatalf("%d gcds of parts up to %d bits", got.gcds, got.widest)
	}
	if len(b) <= k*8300 {
		t.Fatalf("each takes %d bytes", len(b)/k)
	}
	wide := new(big.Int).Lsh(big.NewInt(1), MaxRationalBits)
	two := big.NewInt(2)
	for _, c := range [][2]*big.Int{
		{new(big.Int).Mul(wide, two), new(big.Int).Add(wide, two)},
		{two, new(big.Int).Mul(wide, two)},
		{new(big.Int).Sub(new(big.Int).Neg(wide), big.NewInt(1)), two},
		{big.NewInt(1), new(big.Int).Neg(new(big.Int).Mul(wide, two))},
	} {
		d := counted(rationalBytes(c[0], c[1]))
		var tooWide CanonicalErrorRationalTooWide
		if !errors.As(d.err, &tooWide) || tooWide.Limit != MaxRationalBits {
			t.Errorf("%d / %d bits: %v", bitLength(c[0]), bitLength(c[1]), d.err)
		}
		if d.gcds != 0 {
			t.Errorf("%d / %d bits ran %d gcds", bitLength(c[0]), bitLength(c[1]), d.gcds)
		}
	}
}

// A bignum one bit too wide is refused from its length, for either tag, before its magnitude is built; k of the widest
// build exactly k magnitudes, of 4153 bytes each.
func TestDecodingRefusesAWideBignumBeforeBuildingIt(t *testing.T) {
	edge := new(big.Int).Lsh(big.NewInt(1), MaxIntegerBits)
	for _, n := range []*big.Int{edge, new(big.Int).Sub(new(big.Int).Neg(edge), big.NewInt(1)), new(big.Int).Mul(edge, big.NewInt(255))} {
		d := counted(integerBytes(n))
		var tooWide CanonicalErrorIntegerTooWide
		if !errors.As(d.err, &tooWide) || tooWide.Limit != MaxIntegerBits {
			t.Errorf("%d bits: %v", bitLength(n), d.err)
		}
		if d.magnitudes != 0 {
			t.Errorf("%d bits built %d magnitudes", bitLength(n), d.magnitudes)
		}
	}
	top := must(t)(Integer(new(big.Int).Sub(edge, big.NewInt(1))))
	bottom := must(t)(Integer(new(big.Int).Neg(edge)))
	array := must(t)(Array(top, bottom, top, bottom))
	d := counted(encoded(t, array))
	if d.err != nil || !d.v.Equal(array) || d.magnitudes != 4 || d.widestMagnitude != 4153 {
		t.Fatalf("the widest decode: %v, %d magnitudes of up to %d bytes", d.err, d.magnitudes, d.widestMagnitude)
	}
	part := append(append([]byte{0xd8, 0x1e, 0x82}, integerBytes(edge)...), 0x03)
	if d := counted(part); !isKind(d.err, CanonicalErrorKindRationalTooWide) || d.magnitudes != 0 {
		t.Fatalf("a rational part too wide: %v, %d magnitudes", d.err, d.magnitudes)
	}
}

// The bound holds in the value model for either sign: 2^33219 - 1 and -2^33219 are 33219 bits wide, 2^33219 and
// -2^33219 - 1 are 33220, and a number is checked after it is reduced.
func TestTheIntegerAndRationalBoundsHoldForEitherSign(t *testing.T) {
	edge := new(big.Int).Lsh(big.NewInt(1), MaxIntegerBits)
	one, three := big.NewInt(1), big.NewInt(3)
	for _, n := range []*big.Int{new(big.Int).Sub(edge, one), new(big.Int).Neg(edge)} {
		v := must(t)(Integer(n))
		if v.Weight() != 520 {
			t.Errorf("%d bits weighs %d", bitLength(n), v.Weight())
		}
		if back, err := Decode(encoded(t, v)); err != nil || !back.Equal(v) {
			t.Errorf("%d bits does not round trip: %v", bitLength(n), err)
		}
	}
	for _, n := range []*big.Int{edge, new(big.Int).Sub(new(big.Int).Neg(edge), one)} {
		if _, err := Integer(n); !isKind(err, CanonicalErrorKindIntegerTooWide) {
			t.Errorf("%d bits: %v", bitLength(n), err)
		}
		if _, err := Number(new(big.Int).Mul(n, three), three); !isKind(err, CanonicalErrorKindIntegerTooWide) {
			t.Errorf("%d bits from a ratio: %v", bitLength(n), err)
		}
	}
	if _, err := Number(edge, three); !isKind(err, CanonicalErrorKindRationalTooWide) {
		t.Errorf("2^33219 / 3: %v", err)
	}
	if v, err := Number(new(big.Int).Mul(edge, big.NewInt(2)), edge); err != nil || !v.Equal(Int64(2)) {
		t.Errorf("2^33220 / 2^33219 is 2: %v, %v", v, err)
	}
	if _, ok := RationalOf(edge, three); ok {
		t.Error("RationalOf accepted a part too wide")
	}
	if _, err := Number(one, new(big.Int)); !errors.As(err, new(DivisionByZero)) {
		t.Errorf("division by zero: %v", err)
	}
	for _, c := range [][2]int64{{2, 4}, {-3, 6}, {3, -6}, {0, 5}, {6, 3}} {
		v := must(t)(Number(big.NewInt(c[0]), big.NewInt(c[1])))
		if v.Kind() == KindRational {
			r, _ := v.AsRational()
			if r.Denominator().Cmp(big.NewInt(2)) < 0 || new(big.Int).GCD(nil, nil, new(big.Int).Abs(r.Numerator()), r.Denominator()).Cmp(one) != 0 {
				t.Errorf("%d/%d is not reduced: %v", c[0], c[1], v)
			}
		}
	}
	if Int64(-1<<63).Weight() != 1 || Uint64(1<<64-1).Weight() != 2 {
		t.Error("an integer's weight is 1 + bitLength / 64")
	}
}

// A bignum's form, and that it is present in full, are checked before its width.
func TestABignumIsCheckedForFormAndLengthBeforeWidth(t *testing.T) {
	leadingZero := append([]byte{0xc2, 0x59, 0x10, 0x3a, 0x00}, bytes.Repeat([]byte{0xff}, 4153)...)
	var nc CanonicalErrorNonCanonical
	if _, err := Decode(leadingZero); !errors.As(err, &nc) || nc.Reason != NonCanonicalReasonBignumLeadingZero {
		t.Errorf("a leading zero: %v", err)
	}
	if _, err := Decode([]byte{0xc3, 0x5a, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff}); !isKind(err, CanonicalErrorKindUnexpectedEnd) {
		t.Errorf("truncated: %v", err)
	}
	widest := append([]byte{0xc3, 0x59, 0x10, 0x39, 0x07}, bytes.Repeat([]byte{0xff}, 4152)...)
	if v, err := Decode(widest); err != nil || !v.Equal(must(t)(Integer(new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), MaxIntegerBits))))) {
		t.Errorf("the widest: %v", err)
	}
	past := append([]byte{0xc3, 0x59, 0x10, 0x39, 0x08}, make([]byte, 4152)...)
	if _, err := Decode(past); !isKind(err, CanonicalErrorKindIntegerTooWide) {
		t.Errorf("one past: %v", err)
	}
}

// pieces records what the walk hands a sink: how many bytes, and the longest piece it made itself (a head, or an
// integer's word); a text or byte string of the value is lent in place, never copied.
type pieces struct {
	total       int
	longestMade int
	lent        int
}

func (p *pieces) put(b []byte) bool {
	p.total += len(b)
	p.lent++
	return true
}

func (p *pieces) putString(s string) bool {
	p.total += len(s)
	p.lent++
	return true
}

func (p *pieces) putHead(_ [9]byte, n int) bool {
	p.total += n
	p.longestMade = max(p.longestMade, n)
	return true
}

func (p *pieces) room(uint64) bool { return true }

// The streamed hashing path holds no part of the encoding: whatever the value's size, every piece the walk makes is at
// most 9 bytes (a head or a word), and text and byte strings are lent from the value. The hashing sink then stages at
// most StagedBytes and the hash a block, so memory is independent of the encoding's length.
func TestTheHashingPathBuffersNothingProportionalToTheValue(t *testing.T) {
	for _, n := range []int{10, 1_000, 100_000} {
		items := make([]Value, n)
		entries := make([]Entry, n)
		for i := range items {
			items[i] = Uint64(uint64(i) << 40)
			entries[i] = Entry{Key: "k" + strconv.Itoa(i), Value: Bool(i%2 == 0)}
		}
		wide := must(t)(Array(items...))
		keyed := must(t)(Map(entries...))
		big1 := must(t)(Integer(new(big.Int).Lsh(big.NewInt(1), uint(min(n*8, 33_000)))))
		bigNeg := must(t)(Integer(new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), uint(min(n*8, 33_000))))))
		text := must(t)(Text(string(bytes.Repeat([]byte("é"), n))))
		bs := Bytes(bytes.Repeat([]byte{0x5a}, n))
		for name, v := range map[string]Value{"wide": wide, "keyed": keyed, "big": big1, "negative big": bigNeg, "text": text, "bytes": bs} {
			p := &pieces{}
			writeValue(p, v)
			if p.total != len(encoded(t, v)) {
				t.Errorf("%s of %d: %d bytes walked, %d encoded", name, n, p.total, len(encoded(t, v)))
			}
			if p.longestMade > 9 {
				t.Errorf("%s of %d: the walk made a piece of %d bytes", name, n, p.longestMade)
			}
		}
	}
}

// A value whose weight is over a budget is refused at once, however heavy: a value of 2^60 nodes built in 60 steps.
func TestABudgetRefusesAHeavyValueFromItsWeight(t *testing.T) {
	v := Null()
	for range 60 {
		v = must(t)(Array(v, v))
	}
	if v.Weight() != 1<<61-1 {
		t.Fatalf("weight %d", v.Weight())
	}
	for _, limit := range []int{0, 1000, MaxEncodedBytes, math.MaxInt} {
		_, err := EncodeWithin(v, limit)
		var tooLarge TooLarge
		if !errors.As(err, &tooLarge) || tooLarge.Limit != byteLimit(limit, MaxEncodedBytes) {
			t.Errorf("within %d: %v", limit, err)
		}
	}
	if _, err := Encode(v); !errors.As(err, new(TooLarge)) {
		t.Errorf("Encode: %v", err)
	}
	if _, err := EncodedLengthWithin(v, -5); !errors.As(err, new(TooLarge)) {
		t.Errorf("a negative limit: %v", err)
	}
	if _, err := ValueCommitment(v); !errors.As(err, new(TooLarge)) {
		t.Errorf("ValueCommitment: %v", err)
	}
	tag, _ := NewDomainTag("dilf4s/test/v1")
	if _, err := ContentHash(SHA256, tag, v); !errors.As(err, new(TooLarge)) {
		t.Errorf("ContentHash: %v", err)
	}
	if _, err := RenderJSONWithin(v, MaxRenderBytes); !errors.As(err, new(TooLarge)) {
		t.Errorf("RenderJSONWithin: %v", err)
	}
	// weights saturate, never wrap
	heavier := v
	for range 10 {
		heavier = must(t)(Array(heavier, heavier))
	}
	if heavier.Weight() != MaxWeight {
		t.Errorf("weight %d", heavier.Weight())
	}
}

// A count beyond the input left is refused before anything is allocated, and nesting is a typed error.
func TestCountsAndDepthAreCheckedBeforeAllocation(t *testing.T) {
	if _, err := Decode([]byte{0x9b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff}); !isKind(err, CanonicalErrorKindUnexpectedEnd) {
		t.Errorf("a huge count: %v", err)
	}
	if _, err := Decode([]byte{0xbb, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff}); !isKind(err, CanonicalErrorKindUnexpectedEnd) {
		t.Errorf("a huge map count: %v", err)
	}
	deep := append(bytes.Repeat([]byte{0x81}, MaxDepth), 0x80)
	var de CanonicalErrorDepthExceeded
	if _, err := Decode(deep); !errors.As(err, &de) || de.Limit != MaxDepth {
		t.Errorf("past the depth: %v", err)
	}
	if _, err := Decode(deep[1:]); err != nil {
		t.Errorf("at the depth: %v", err)
	}
	if _, err := DecodeWithLimit([]byte{0x80}, -3); !errors.As(err, &de) || de.Limit != 0 {
		t.Errorf("a negative limit: %v", err)
	}
	// a nesting far past the limit, refused at its 257th head without a stack overflow
	if _, err := Decode(bytes.Repeat([]byte{0x81}, 1_000_000)); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Errorf("a million levels: %v", err)
	}
}

// Decoded byte strings and text are copies: the input is never kept.
func TestDecodingKeepsNoViewOfItsInput(t *testing.T) {
	input := []byte{0x82, 0x43, 1, 2, 3, 0x61, 'a'}
	v, err := Decode(input)
	if err != nil {
		t.Fatal(err)
	}
	input[2], input[6] = 9, 'z'
	b, _ := mustIndex(t, v, 0).AsBytes()
	s, _ := mustIndex(t, v, 1).AsText()
	if b[0] != 1 || s != "a" {
		t.Fatal("the decoded value shares its input")
	}
}

func mustIndex(t testing.TB, v Value, i int) Value {
	t.Helper()
	x, ok := v.Index(i)
	if !ok {
		t.Fatalf("no element %d", i)
	}
	return x
}
