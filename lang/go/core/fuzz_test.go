package core

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

// vectorStrings is every text at a member named key in the vector file, walked with an explicit stack: the seed
// corpus of the fuzz targets, read with this package's own parser (its tests cannot import the generated models,
// which import it).
func vectorStrings(f *testing.F, file, key string) []string {
	f.Helper()
	b, err := os.ReadFile(filepath.Join("../../../shared/vectors", file))
	if err != nil {
		f.Fatal(err)
	}
	root, err := ParseJSON(string(b))
	if err != nil {
		f.Fatal(err)
	}
	var out []string
	todo := []Value{root}
	for len(todo) > 0 {
		v := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		switch v.Kind() {
		case KindArray:
			todo = append(todo, v.Items()...)
		case KindMap:
			for k, x := range v.Entries() {
				if s, ok := x.AsText(); ok && k == key {
					out = append(out, s)
				}
				todo = append(todo, x)
			}
		}
	}
	return out
}

func canonicalError(err error) bool {
	var ce CanonicalError
	return errors.As(err, &ce)
}

// FuzzDecodeCBOR: the decoder never panics, refuses only with a CanonicalError, and accepts only canonical bytes, so
// what it accepts re-encodes to the same bytes.
func FuzzDecodeCBOR(f *testing.F) {
	for _, file := range []string{"canonical_encoding_v1.json", "merkle_v1.json"} {
		for _, key := range []string{"cbor", "value"} {
			for _, h := range vectorStrings(f, file, key) {
				if b, err := FromHex(h); err == nil {
					f.Add(b)
				}
			}
		}
	}
	f.Add([]byte{})
	f.Fuzz(func(t *testing.T, b []byte) {
		v, err := Decode(b)
		if err != nil {
			if !canonicalError(err) {
				t.Fatalf("refused with %T: %v", err, err)
			}
			return
		}
		again, err := Encode(v)
		if err != nil || string(again) != string(b) {
			t.Fatalf("decoded %x re-encodes as %x, %v", b, again, err)
		}
		if back, err := Decode(again); err != nil || !back.Equal(v) {
			t.Fatalf("decoded %x decodes back as %v, %v", b, back, err)
		}
		if n, err := EncodedLengthWithin(v, len(b)); err != nil || n != len(b) {
			t.Fatalf("decoded %x measures %d, %v", b, n, err)
		}
	})
}

// FuzzParseJSON: the parser never panics and refuses only with a CanonicalError; what it accepts renders (unless JSON
// cannot hold it, refused as a CanonicalError or TooLarge) and reads back as the same value.
func FuzzParseJSON(f *testing.F) {
	for _, file := range []string{"canonical_encoding_v1.json", "diagnostics_v1.json"} {
		for _, s := range vectorStrings(f, file, "json") {
			f.Add(s)
		}
	}
	for _, s := range vectorStrings(f, "canonical_encoding_v1.json", "render") {
		f.Add(s)
	}
	f.Add("")
	f.Fuzz(func(t *testing.T, text string) {
		v, err := ParseJSON(text)
		if err != nil {
			if !canonicalError(err) {
				t.Fatalf("refused with %T: %v", err, err)
			}
			return
		}
		out, err := RenderJSONWithin(v, MaxRenderBytes)
		if err != nil {
			if !canonicalError(err) && !errors.As(err, new(TooLarge)) {
				t.Fatalf("not rendered, with %T: %v", err, err)
			}
			return
		}
		back, err := ParseJSON(out)
		if err != nil || !back.Equal(v) {
			t.Fatalf("%q renders as %q, which reads back as %v, %v", text, out, back, err)
		}
	})
}
