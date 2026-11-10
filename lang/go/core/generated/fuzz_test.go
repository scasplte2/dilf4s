package generated_test

import (
	"errors"
	"testing"

	"github.com/scasplte2/dilf4s/lang/go/core"
)

// FuzzModels drives every generated model's decoder: the first two bytes pick the model, the rest is CBOR. A decoder
// never panics and refuses only with a DecodeFailure; what it accepts encodes back to the same value, so no model's
// encoder refuses what its decoder accepted. Seeded with every vector file, under its schema's model.
func FuzzModels(f *testing.F) {
	all := models()
	for file, rule := range schemas() {
		value, err := core.ParseJSON(readVectors(f, file))
		if err != nil {
			f.Fatal(err)
		}
		b, err := core.Encode(value)
		if err != nil {
			f.Fatal(err)
		}
		for i, m := range all {
			if m.rule == rule {
				f.Add(append([]byte{byte(i >> 8), byte(i)}, b...))
			}
		}
	}
	f.Add([]byte{0, 0, 0xf6})
	f.Fuzz(func(t *testing.T, data []byte) {
		if len(data) < 2 {
			return
		}
		m := all[(int(data[0])<<8|int(data[1]))%len(all)]
		v, err := core.Decode(data[2:])
		if err != nil {
			return
		}
		again, err := m.roundTrip(v)
		var failure core.DecodeFailure
		var ce core.CanonicalError
		switch {
		case err == nil:
			if !again.Equal(v) {
				t.Fatalf("%s: %v round trips as %v", m.rule, v, again)
			}
		case errors.As(err, &ce):
			t.Fatalf("%s: decoded, then refused to encode: %v", m.rule, err)
		case !errors.As(err, &failure):
			t.Fatalf("%s: refused with %T: %v", m.rule, err, err)
		}
	})
}
