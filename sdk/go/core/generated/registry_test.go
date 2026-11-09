package generated_test

import (
	"errors"
	"testing"

	"github.com/scasplte2/dilf4s/sdk/go/core"
)

// modelCodec is a generated model by its rule, as a decode-then-encode round trip of a canonical value.
type modelCodec struct {
	rule      string
	roundTrip func(core.Value) (core.Value, error)
}

// model is the round trip of a model's decoder and encoder.
func model[T any](rule string, dec func(core.Value) (T, error), enc func(T, core.Encoding) (core.Value, error)) modelCodec {
	return modelCodec{rule: rule, roundTrip: func(v core.Value) (core.Value, error) {
		x, err := dec(v)
		if err != nil {
			return core.Value{}, err
		}
		return enc(x, core.Encoding{})
	}}
}

// The registry lists every model once.
func TestTheRegistryListsEachModelOnce(t *testing.T) {
	seen := map[string]bool{}
	for _, m := range models() {
		if seen[m.rule] {
			t.Fatalf("%s twice", m.rule)
		}
		seen[m.rule] = true
	}
	if len(seen) < 250 {
		t.Fatalf("only %d models", len(seen))
	}
}

// Every model's decoder takes the values of every kind: it refuses with a DecodeFailure, never a panic or another
// error, and what it accepts its encoder writes back as the same value.
func TestEveryModelDecodesOrRefusesEveryKind(t *testing.T) {
	text, _ := core.Text("x")
	empty, _ := core.Array()
	emptyMap, _ := core.Map()
	keyed, _ := core.Map(core.Entry{Key: "kind", Value: text})
	for _, m := range models() {
		for _, v := range []core.Value{core.Null(), core.Bool(true), core.Int64(-1), core.Uint64(1 << 63), text, core.Bytes(nil), empty, emptyMap, keyed} {
			again, err := m.roundTrip(v)
			var failure core.DecodeFailure
			switch {
			case err == nil:
				if !again.Equal(v) {
					t.Errorf("%s: %v round trips as %v", m.rule, v, again)
				}
			case !errors.As(err, &failure):
				t.Errorf("%s: %v refused with %T: %v", m.rule, v, err, err)
			}
		}
	}
}
