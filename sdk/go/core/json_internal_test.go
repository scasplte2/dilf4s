package core

import (
	"errors"
	"math/big"
	"strconv"
	"strings"
	"testing"
)

// count records what reached the output, as counts alone: bytes, pieces, and the longest piece.
type count struct {
	bytes, pieces, longest int
}

func (c *count) WriteString(s string) (int, error) {
	c.bytes += len(s)
	c.pieces++
	c.longest = max(c.longest, len(s))
	return len(s), nil
}

func counts(t *testing.T, v Value) count {
	t.Helper()
	var c count
	if err := writeJSON(&renderBudget{out: &c, max: MaxRenderBytes}, v); err != nil {
		t.Fatal(err)
	}
	return c
}

// Each byte of a rendering is written once, whatever the value's depth or width: counted, not timed, for a depth-256
// chain of arrays, a depth-256 chain of maps with escaped text at the bottom, and a 20,000-entry map.
func TestEveryByteOfARenderingIsWrittenOnce(t *testing.T) {
	chain := Int64(7)
	for range MaxDepth {
		chain = must(t)(Array(chain))
	}
	mapChain := must(t)(Text("a\"b\n\x01é😀"))
	for i := range MaxDepth {
		mapChain = must(t)(Map(Entry{Key: "k" + strconv.Itoa(i), Value: mapChain}, Entry{Key: "z", Value: Int64(int64(i))}))
	}
	entries := make([]Entry, 20_000)
	for i := range entries {
		v := Int64(int64(i))
		if i%2 == 1 {
			v = must(t)(Text("v\t" + strconv.Itoa(i)))
		}
		entries[i] = Entry{Key: "key" + strconv.Itoa(100_000 + i)[1:], Value: v}
	}
	wide := must(t)(Map(entries...))
	for _, c := range []struct {
		name    string
		v       Value
		longest int
	}{{"array chain", chain, 1}, {"map chain", mapChain, len("é😀")}, {"wide map", wide, len("key00000")}} {
		text, err := RenderJSONWithin(c.v, MaxRenderBytes)
		if err != nil {
			t.Fatal(err)
		}
		got := counts(t, c.v)
		if got.bytes != len(text) {
			t.Errorf("%s: %d bytes written for a rendering of %d", c.name, got.bytes, len(text))
		}
		if uint64(got.pieces) > 4*c.v.Weight() {
			t.Errorf("%s: %d pieces for weight %d", c.name, got.pieces, c.v.Weight())
		}
		if got.longest != c.longest {
			t.Errorf("%s: the longest piece is %d", c.name, got.longest)
		}
	}
	if got := counts(t, chain); got != (count{bytes: 513, pieces: 513, longest: 1}) {
		t.Errorf("the array chain: %+v", got)
	}
}

// Factoring a denominator takes at most four big-number divisions and exponentiations, whatever its width, and the
// scale bound then refuses 1/2^33218 and 1/5^14306 before any digit is produced.
func TestFactoringADenominatorIsBounded(t *testing.T) {
	pow := func(b, e int64) *big.Int { return new(big.Int).Exp(big.NewInt(b), big.NewInt(e), nil) }
	mul := func(a, b *big.Int) *big.Int { return new(big.Int).Mul(a, b) }
	for _, c := range []struct {
		d           *big.Int
		twos, fives uint64
		terminates  bool
	}{
		{pow(2, 33_219), 33_219, 0, true},
		{pow(5, 14_306), 0, 14_306, true},
		{mul(pow(2, 3), pow(5, 9_000)), 3, 9_000, true},
		{mul(pow(5, 14_000), big.NewInt(3)), 0, 0, false},
		{mul(pow(2, 100), big.NewInt(3)), 0, 0, false},
		{big.NewInt(5), 0, 1, true},
		{big.NewInt(1), 0, 0, true},
	} {
		twos, fives, ok, steps := twosAndFives(c.d)
		if ok != c.terminates || (ok && (twos != c.twos || fives != c.fives)) {
			t.Errorf("%d bits: %d, %d, %v", c.d.BitLen(), twos, fives, ok)
		}
		if steps > 4 {
			t.Errorf("%d bits: %d steps", c.d.BitLen(), steps)
		}
	}
	for _, b := range append(intRange(1, 300), 1_000, 4_096, 9_999, 10_000, 10_001, 14_306) {
		_, fives, ok, steps := twosAndFives(pow(5, int64(b)))
		if !ok || fives != uint64(b) || steps > 4 {
			t.Errorf("5^%d: %d, %v in %d steps", b, fives, ok, steps)
		}
	}
	if _, _, _, steps := twosAndFives(pow(5, 14_306)); steps != 2 {
		t.Errorf("an exact estimate takes %d steps", steps)
	}
	for _, d := range []*big.Int{pow(2, 33_218), pow(5, 14_306)} {
		r, ok := must(t)(Number(big.NewInt(1), d)).AsRational()
		if !ok {
			t.Fatal("not a rational")
		}
		var nj CanonicalErrorNotJsonRepresentable
		if _, err := Decimal(r); !errors.As(err, &nj) || nj.What != NotJsonOverLongNumber {
			t.Errorf("%d bits: %v", d.BitLen(), err)
		}
	}
}

func intRange(lo, hi int) []int {
	var out []int
	for i := lo; i <= hi; i++ {
		out = append(out, i)
	}
	return out
}

// Reducing by two and five is the gcd reduction, for every shape of shared factor.
func TestReducingADecimalIsTheGcdReduction(t *testing.T) {
	ten := big.NewInt(10)
	numerators := []*big.Int{}
	for _, n := range []int64{0, 1, -1, 2, 5, 10, 16, 25, 125, 640, 3125, -7_812_500, 999, 1_000_000, 2_048_000_000} {
		numerators = append(numerators, big.NewInt(n))
	}
	numerators = append(numerators, new(big.Int).Exp(big.NewInt(5), big.NewInt(20_000), nil),
		new(big.Int).Mul(new(big.Int).Exp(big.NewInt(2), big.NewInt(20_000), nil), big.NewInt(3)))
	for _, n := range numerators {
		for _, scale := range []int{0, 1, 2, 3, 7, 8, 9, 64, 1000, 10_000} {
			general, gerr := Number(n, new(big.Int).Exp(ten, big.NewInt(int64(scale)), nil))
			fast, ferr := decimal(new(big.Int).Set(n), scale)
			if (gerr == nil) != (ferr == nil) || (gerr == nil && !general.Equal(fast)) {
				t.Errorf("%d bits / 10^%d: %v, %v and %v, %v", n.BitLen(), scale, general, gerr, fast, ferr)
			}
		}
	}
}

// A container opened while MaxDepth are open is refused at its bracket, an empty one too; the parser keeps its own
// stack, so a nesting far past the limit is a typed error.
func TestJSONDepth(t *testing.T) {
	open := strings.Repeat("[", MaxDepth)
	if _, err := ParseJSON(open + "{}" + strings.Repeat("]", MaxDepth)); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Errorf("past the depth: %v", err)
	}
	if _, err := ParseJSON(open[1:] + "{}" + strings.Repeat("]", MaxDepth-1)); err != nil {
		t.Errorf("at the depth: %v", err)
	}
	if _, err := ParseJSON(strings.Repeat("[", 1_000_000)); !isKind(err, CanonicalErrorKindDepthExceeded) {
		t.Errorf("a million levels: %v", err)
	}
}

// Go text is bytes: a string that is not well-formed UTF-8 is no text, refused at its first ill-formed byte, as JSON
// text before it is parsed, as a value and as a map key.
func TestTextThatIsNotUTF8IsRefused(t *testing.T) {
	var bad CanonicalErrorInvalidUtf8
	for _, c := range []struct {
		text   string
		offset uint64
	}{{"\xff", 0}, {`"a` + "\x80" + `"`, 2}, {"[1,\xed\xa0\x80]", 3}, {"\"\xc0\x80\"", 1}} {
		if _, err := ParseJSON(c.text); !errors.As(err, &bad) || bad.Offset != c.offset {
			t.Errorf("JSON %q: %v", c.text, err)
		}
	}
	if _, err := Text("ok\xe2\x82"); !errors.As(err, &bad) || bad.Offset != 2 {
		t.Errorf("text: %v", err)
	}
	if _, err := Map(Entry{Key: "\xff", Value: Int64(1)}); !errors.As(err, &bad) {
		t.Errorf("a key: %v", err)
	}
	if _, err := EncodeText("\xfe", Encoding{}); !errors.As(err, &bad) {
		t.Errorf("a model's text: %v", err)
	}
	if _, err := (TextMapCodec[uint64]{Enc: EncodeUint, Dec: DecodeUint}).Encode(TextMapFrom(map[string]uint64{"\xff": 1}), Encoding{}); !errors.As(err, &bad) {
		t.Errorf("a text map's key: %v", err)
	}
}

// A number's digits are counted on its text before any conversion, and its scale checked: the refusal holds its size
// and offset, never its digits.
func TestJSONNumberBounds(t *testing.T) {
	var invalid CanonicalErrorInvalidNumber
	long := strings.Repeat("9", MaxDigits+1)
	if _, err := ParseJSON("[" + long + "]"); !errors.As(err, &invalid) {
		t.Fatalf("10001 digits: %v", err)
	}
	bound, _ := invalid.Bound.Get()
	if d, ok := bound.(NumberBoundDigits); !ok || d.Digits != MaxDigits+1 || d.Limit != MaxDigits {
		t.Errorf("the bound %v", invalid.Bound)
	}
	if l, _ := invalid.Length.Get(); l != MaxDigits+1 {
		t.Errorf("the length %v", invalid.Length)
	}
	if o, _ := invalid.Offset.Get(); o != 1 {
		t.Errorf("the offset %v", invalid.Offset)
	}
	if _, err := ParseJSON("1e10001"); !errors.As(err, &invalid) {
		t.Fatalf("past the scale: %v", err)
	}
	bound, _ = invalid.Bound.Get()
	if s, ok := bound.(NumberBoundScale); !ok || s.Scale != -10001 || s.Limit != DefaultMaxScale {
		t.Errorf("the scale bound %v", invalid.Bound)
	}
	if _, err := ParseJSON("1e" + strings.Repeat("9", 40)); !errors.As(err, &invalid) {
		t.Fatalf("a huge exponent: %v", err)
	}
	bound, _ = invalid.Bound.Get()
	if s, ok := bound.(NumberBoundScale); !ok || s.Scale != -1<<31 {
		t.Errorf("the scale saturates: %v", invalid.Bound)
	}
	if _, err := ParseJSON("9e9999"); !isKind(err, CanonicalErrorKindIntegerTooWide) {
		t.Errorf("9e9999: %v", err)
	}
	if v, err := ParseJSON("8e9999"); err != nil || v.Kind() != KindInt {
		t.Errorf("8e9999: %v", err)
	}
	if _, err := ParseJSONWithMaxScale("1", DefaultMaxScale+1); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("a raised scale bound: %v", err)
	}
	if _, err := ParseJSONWithMaxScale("0.5", 0); !errors.As(err, &invalid) {
		t.Errorf("a lowered scale bound: %v", err)
	}
	// 0. then 10000 zeros then 2e1 is 2/10^10000: within every bound, its leading zeros never converted
	if v, err := ParseJSON("0." + strings.Repeat("0", 10_000) + "2e1"); err != nil || v.Kind() != KindRational {
		t.Errorf("a long fraction: %v, %v", v.Kind(), err)
	}
}

// Writing and reading back never fails, and neither does reading and writing back.
func TestJSONRoundTrips(t *testing.T) {
	for _, text := range []string{`0.1`, `-0`, `1.0`, `9007199254740993`, `{"b":1,"a":[null,true,"x\u0000"]}`, `"😀"`, `1e3`, `-2.50e-3`} {
		v, err := ParseJSON(text)
		if err != nil {
			t.Fatalf("%s: %v", text, err)
		}
		out, err := RenderJSONWithin(v, MaxRenderBytes)
		if err != nil {
			t.Fatalf("%s: %v", text, err)
		}
		back, err := ParseJSON(out)
		if err != nil || !back.Equal(v) {
			t.Errorf("%s renders as %s, which reads back as %v, %v", text, out, back, err)
		}
	}
	if _, err := RenderJSONWithin(Bytes([]byte{1}), 100); !isKind(err, CanonicalErrorKindNotJsonRepresentable) {
		t.Errorf("bytes: %v", err)
	}
	third := must(t)(Number(big.NewInt(1), big.NewInt(3)))
	var nj CanonicalErrorNotJsonRepresentable
	if _, err := RenderJSONWithin(third, 100); !errors.As(err, &nj) || nj.What != NotJsonNonTerminatingRational {
		t.Errorf("1/3: %v", err)
	}
	if _, err := Decimal(Rational{}); !isKind(err, CanonicalErrorKindTypeMismatch) {
		t.Errorf("the zero Rational: %v", err)
	}
}

// An escaped text's length is counted no further than one escape past the limit, so it cannot overflow an int (on a
// 32-bit one, 357,913,941 NULs escape to 2^31 bytes).
func TestAnEscapedLengthStopsAtTheLimit(t *testing.T) {
	nuls := strings.Repeat("\x00", 1000)
	if n, fits := quotedLenWithin(nuls, 6002); !fits || n != 6002 {
		t.Errorf("within: %d, %v", n, fits)
	}
	if n, fits := quotedLenWithin(nuls, 6001); fits || n > 6001+6 {
		t.Errorf("one under: %d, %v", n, fits)
	}
	if n, fits := quotedLenWithin(strings.Repeat("\x00", 1_000_000), 10); fits || n > 16 {
		t.Errorf("far over: counted to %d", n)
	}
}
