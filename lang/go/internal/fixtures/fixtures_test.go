package fixtures_test

import (
	"errors"
	"math/big"
	"testing"

	"github.com/scasplte2/dilf4s/lang/go/core"
	"github.com/scasplte2/dilf4s/lang/go/internal/fixtures/fixturea"
	"github.com/scasplte2/dilf4s/lang/go/internal/fixtures/fixtureb"
)

// modelCodec is a generated model by its rule, as a decode-then-encode round trip of a canonical value.
type modelCodec struct {
	rule      string
	roundTrip func(core.Value) (core.Value, error)
}

func model[T any](rule string, dec func(core.Value) (T, error), enc func(T, core.Encoding) (core.Value, error)) modelCodec {
	return modelCodec{rule: rule, roundTrip: func(v core.Value) (core.Value, error) {
		x, err := dec(v)
		if err != nil {
			return core.Value{}, err
		}
		return enc(x, core.Encoding{})
	}}
}

// roundTrip encodes x, decodes the value, and encodes the result again: both values must be equal.
func roundTrip[T any](t *testing.T, x T, enc func(T, core.Encoding) (core.Value, error), dec func(core.Value) (T, error)) T {
	t.Helper()
	v, err := enc(x, core.Encoding{})
	if err != nil {
		t.Fatalf("encodes: %v", err)
	}
	back, err := dec(v)
	if err != nil {
		t.Fatalf("decodes: %v", err)
	}
	again, err := enc(back, core.Encoding{})
	if err != nil || !again.Equal(v) {
		t.Fatalf("encodes back as %v, %v", again, err)
	}
	return back
}

func big128() core.Int {
	n, _ := new(big.Int).SetString("340282366920938463463374607431768211455", 10)
	i, _ := core.IntFromBig(n)
	return i
}

// Every construct of the profile round-trips through the generated model.
func TestEverythingRoundTrips(t *testing.T) {
	m, err := core.NewTextMap(core.TextEntry[fixturea.Items]{Key: "k", Value: fixturea.Items{{X: 1}, {X: 2}}}, core.TextEntry[fixturea.Items]{Key: "a", Value: nil})
	if err != nil {
		t.Fatal(err)
	}
	any, _ := core.Text("anything")
	x := fixtureb.Everything{
		U: 1<<64 - 1, I: core.IntOf(-1 << 62), N: core.IntOf(-7), U32: 255, I32: -5, U64: 1<<64 - 1, I64: -1 << 63, Big: big128(),
		Bytes: []byte{1, 2}, Fixed: [4]byte{9, 8, 7, 6}, Sized: []byte{1}, BoundedByLimit: make([]byte, 1024),
		Text: "é", SizedText: "12345678", Hex: "00ff", Hex0x: "0x", Decimal: "0", StateKey: "a/b.c_d-e",
		Flag: true, Any: any, List: [][]uint64{{}, {1, 2}}, Bounded: []string{"x"}, Map: m,
		Maybe: core.Some(fixtureb.WideLow), Choice: core.Some[fixtureb.Choice](fixtureb.ChoiceMode{Value: fixturea.ModeOff}),
		Mode: fixturea.ModeOn, Op: fixtureb.OpEq, Items: fixturea.Items{{X: 3}},
	}
	back := roundTrip(t, x, fixtureb.EncodeEverything, fixtureb.DecodeEverything)
	if back.Fixed != x.Fixed || back.Big.Cmp(x.Big) != 0 || back.Map.Len() != 2 || back.Op != fixtureb.OpEq {
		t.Fatalf("decoded as %+v", back)
	}
	v, _ := fixtureb.EncodeEverything(x, core.Encoding{})
	if version, _ := v.Get("version"); !version.Equal(core.Int64(1)) {
		t.Fatal("the literal field is written")
	}
	// each bound refuses one past it, as its decoder would
	for name, bad := range map[string]fixtureb.Everything{
		"a uint32 past 255":      with(x, func(e *fixtureb.Everything) { e.U32 = 256 }),
		"an int32 past 5":        with(x, func(e *fixtureb.Everything) { e.I32 = 6 }),
		"a sized byte string":    with(x, func(e *fixtureb.Everything) { e.Sized = nil }),
		"bytes past the limit":   with(x, func(e *fixtureb.Everything) { e.BoundedByLimit = make([]byte, 1025) }),
		"sized text":             with(x, func(e *fixtureb.Everything) { e.SizedText = "123456789" }),
		"hex":                    with(x, func(e *fixtureb.Everything) { e.Hex = "0F" }),
		"0x hex":                 with(x, func(e *fixtureb.Everything) { e.Hex0x = "00" }),
		"a decimal":              with(x, func(e *fixtureb.Everything) { e.Decimal = "01" }),
		"a state key":            with(x, func(e *fixtureb.Everything) { e.StateKey = "a//b" }),
		"too many":               with(x, func(e *fixtureb.Everything) { e.Bounded = []string{"a", "b", "c", "d"} }),
		"too few":                with(x, func(e *fixtureb.Everything) { e.Bounded = nil }),
		"a nint":                 with(x, func(e *fixtureb.Everything) { e.N = core.IntOf(0) }),
		"a mode that is not one": with(x, func(e *fixtureb.Everything) { e.Mode = "sideways" }),
		"the zero enumeration":   with(x, func(e *fixtureb.Everything) { e.Maybe = core.Some(fixtureb.Wide(0)) }),
		"a nil choice":           with(x, func(e *fixtureb.Everything) { e.Choice = core.Some[fixtureb.Choice](nil) }),
		"text that is not UTF-8": with(x, func(e *fixtureb.Everything) { e.Text = "\xff" }),
	} {
		if _, err := fixtureb.EncodeEverything(bad, core.Encoding{}); err == nil {
			t.Errorf("%s encoded", name)
		}
	}
}

func with(x fixtureb.Everything, change func(*fixtureb.Everything)) fixtureb.Everything {
	change(&x)
	return x
}

// A choice is told apart by Go type when encoding, and by keys or text when decoding; another package's record and
// choice of texts are wrapped.
func TestChoiceAlternatives(t *testing.T) {
	for _, c := range []fixtureb.Choice{
		fixtureb.ChoiceItem{Value: fixturea.Item{X: 5}},
		fixtureb.Local{Y: 6},
		fixtureb.ChoiceEmpty{},
		fixtureb.ChoiceMode{Value: fixturea.ModeOn},
	} {
		if back := roundTrip(t, c, fixtureb.EncodeChoice, fixtureb.DecodeChoice); back != c {
			t.Errorf("%#v decodes as %#v", c, back)
		}
	}
	if _, err := fixtureb.EncodeChoice(fixtureb.ChoiceMode{Value: "sideways"}, core.Encoding{}); err == nil {
		t.Error("a wrapped mode that is not one encoded")
	}
}

// A record holding itself by value holds a pointer: a chain encodes, and one nested past the depth limit is a typed
// error, not a stack overflow.
func TestARecordHoldingItselfIsHeldThroughAPointer(t *testing.T) {
	leaf := fixtureb.Node{Value: 1}
	n := fixtureb.Node{Value: 2, Next: core.Some(&leaf), Children: []fixtureb.Node{leaf}}
	back := roundTrip(t, n, fixtureb.EncodeNode, fixtureb.DecodeNode)
	next, _ := back.Next.Get()
	if next == nil || next.Value != 1 || next == &leaf {
		t.Fatal("the decoded chain is a fresh copy")
	}
	deep := fixtureb.Node{}
	for range 10_000 {
		prev := deep
		deep = fixtureb.Node{Next: core.Some(&prev)}
	}
	var de core.CanonicalErrorDepthExceeded
	if _, err := fixtureb.EncodeNode(deep, core.Encoding{}); !errors.As(err, &de) {
		t.Fatalf("a deep chain: %v", err)
	}
	if _, err := fixtureb.EncodePong(fixtureb.Pong{}, core.Encoding{}); err == nil {
		t.Fatal("a nil required pointer encoded")
	}
	p := roundTrip(t, fixtureb.Pong{Ping: &fixtureb.Ping{}}, fixtureb.EncodePong, fixtureb.DecodePong)
	if p.Ping == nil {
		t.Fatal("a mutual record decodes")
	}
}

// A model can hold one value in two fields, so a tree of 60 levels stands for 2^60 leaves: encoding charges each node
// it builds against the budget, and refuses it as TooLarge once the budget is built, never expanding it.
func TestAForkedModelIsBoundedByTheBudget(t *testing.T) {
	var b fixtureb.Branch = fixtureb.Leaf{Leaf: 1}
	for range 60 {
		b = fixtureb.Tree{Left: b, Right: b}
	}
	var tooLarge core.TooLarge
	if _, err := fixtureb.EncodeBranch(b, core.EncodingWithin(10_000)); !errors.As(err, &tooLarge) || tooLarge.Limit != 10_000 {
		t.Fatalf("within 10,000: %v", err)
	}
	small := fixtureb.Tree{Left: fixtureb.Leaf{Leaf: 1}, Right: fixtureb.Leaf{Leaf: 2}}
	if back := roundTrip(t, fixtureb.Branch(small), fixtureb.EncodeBranch, fixtureb.DecodeBranch); back != fixtureb.Branch(small) {
		t.Fatalf("a small tree decodes as %#v", back)
	}
}

// An enumeration's cases are numbered from 1 and written as their integers, however wide.
func TestAWideEnumeration(t *testing.T) {
	low, _ := new(big.Int).SetString("-18446744073709551616", 10)
	for _, c := range []struct {
		w    fixtureb.Wide
		wire *big.Int
	}{{fixtureb.WideLow, low}, {fixtureb.WideHigh, new(big.Int).SetUint64(1<<64 - 1)}, {fixtureb.WideSmall, big.NewInt(3)}} {
		v, err := fixtureb.EncodeWide(c.w, core.Encoding{})
		if err != nil {
			t.Fatal(err)
		}
		if i, _ := v.AsInt(); i.Big().Cmp(c.wire) != 0 {
			t.Errorf("%d is written as %v", c.w, i)
		}
		if back, err := fixtureb.DecodeWide(v); err != nil || back != c.w {
			t.Errorf("%v decodes as %v, %v", v, back, err)
		}
	}
	if _, err := fixtureb.DecodeWide(core.Int64(4)); err == nil {
		t.Error("4 is no case")
	}
	if !fixtureb.Wide(0).Wire().Equal(core.IntOf(0)) || fixtureb.Wide(0).Valid() {
		t.Error("the zero value is no case")
	}
}

// An error family's kinds are structs checkable with errors.As; a kind wrapping another package's error unwraps to it.
func TestErrorFamilies(t *testing.T) {
	var err error = fixtureb.OuterErrorWrap{Err: fixturea.InnerErrorGone{Name: core.PreviewOf("x")}}
	var outer fixtureb.OuterError
	if !errors.As(err, &outer) || outer.Kind() != fixtureb.OuterErrorKindWrap {
		t.Fatalf("not an OuterError: %v", err)
	}
	var gone fixturea.InnerErrorGone
	if !errors.As(err, &gone) || gone.Name.Head() != "x" {
		t.Fatalf("does not unwrap to its cause: %v", err)
	}
	var inner fixturea.InnerError
	if !errors.As(err, &inner) || inner.Kind() != fixturea.InnerErrorKindGone {
		t.Fatal("the cause is an InnerError")
	}
	bare := fixtureb.OuterErrorBare{Limit: core.Some(uint32(7)), Mode: fixturea.ModeOff}
	if bare.Error() != "Bare (limit: 7, mode: off)" {
		t.Errorf("message %q", bare.Error())
	}
	if (fixtureb.OuterErrorWrap{}).Error() == "" || (fixtureb.OuterErrorWrap{}).Unwrap() != nil {
		t.Error("a zero kind renders, and unwraps to nothing")
	}
	if len(fixtureb.AllOuterErrorKind()) != 2 {
		t.Error("every kind")
	}
}

// The constants are untyped: each takes the type of where it is used, if it fits.
func TestConstants(t *testing.T) {
	var small uint16 = fixtureb.MaxSmall
	var wide uint64 = fixtureb.MaxWide
	if small != 1024 || wide != 1<<64-1 {
		t.Fatal("constants")
	}
}

// Every model's decoder refuses with a DecodeFailure or accepts, and what it accepts it writes back.
func TestEveryFixtureModelRoundTripsWhatItDecodes(t *testing.T) {
	text, _ := core.Text("on")
	empty, _ := core.Map()
	for _, m := range models() {
		for _, v := range []core.Value{core.Null(), core.Int64(3), text, empty} {
			again, err := m.roundTrip(v)
			var failure core.DecodeFailure
			if err == nil && !again.Equal(v) {
				t.Errorf("%s: %v round trips as %v", m.rule, v, again)
			} else if err != nil && !errors.As(err, &failure) {
				t.Errorf("%s: refused with %T", m.rule, err)
			}
		}
	}
}
