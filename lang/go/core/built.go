package core

import (
	"slices"
	"strconv"
)

// BuildStep is a step of a built-value recipe (shared/schemas/refusal-vectors.cddl, jlvm-vectors.cddl): Wrap replaces,
// Times times over, every text "$" in Template (canonical CBOR) by the value so far; a step with Copies set makes an
// array of that many of it.
type BuildStep struct {
	Template []byte
	Times    uint64
	Copies   Optional[uint64]
}

// MaxCopies is the most copies a built-value step makes: the array of copies is allocated, one handle each, so it is
// bounded, at MaxProofWeight (an array of more copies weighs more than any verifier accepts).
const MaxCopies = MaxProofWeight

// Build is the value a recipe describes: its leaf (canonical CBOR), then each step in order. Every occurrence is the
// one value, shared, so building costs the recipe and the copies' arrays, never the weight (a built value can weigh up
// to 2^63 - 1), and nothing here encodes the built value. A leaf or template that is not canonical CBOR is its
// CanonicalError; a value past the depth limit CanonicalErrorDepthExceeded; more than MaxCopies copies a
// CanonicalErrorTypeMismatch. Never recursive: a template is rebuilt with an explicit stack. The work is bounded
// however large a step's times: a template without a "$" gives itself, the template "$" gives the value unchanged,
// and any other places the value inside a container, so each wrap is deeper than the last (but a first one placing
// null where a map drops it) and the depth limit ends it within MaxDepth + 1 wraps.
func Build(leaf []byte, steps []BuildStep) (Value, error) {
	v, err := Decode(leaf)
	if err != nil {
		return Value{}, err
	}
	for _, s := range steps {
		if n, ok := s.Copies.Get(); ok {
			if n > MaxCopies {
				return Value{}, typeMismatch(strconv.FormatUint(n, 10) + " copies, over " + strconv.Itoa(MaxCopies))
			}
			items := make([]Value, n)
			for i := range items {
				items[i] = v
			}
			if v, err = arrayOf(items); err != nil {
				return Value{}, err
			}
			continue
		}
		t, err := Decode(s.Template)
		if err != nil {
			return Value{}, err
		}
		if s.Times == 0 {
			continue
		}
		if text, ok := t.AsText(); ok && text == "$" {
			continue
		}
		if !holdsPlaceholder(t) {
			v = t
			continue
		}
		for range s.Times {
			if v, err = fill(t, v); err != nil {
				return Value{}, err
			}
		}
	}
	return v, nil
}

// holdsPlaceholder reports whether the template t holds the text "$" anywhere, walked with an explicit stack.
func holdsPlaceholder(t Value) bool {
	todo := []Value{t}
	for len(todo) > 0 {
		v := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		switch v.Kind() {
		case KindText:
			if v.n.text == "$" {
				return true
			}
		case KindArray:
			todo = append(todo, v.n.items...)
		case KindMap:
			todo = append(todo, v.n.vals...)
		}
	}
	return false
}

// buildFrame is a node of a template still to rebuild, or a container whose parts are rebuilt and waiting.
type buildFrame struct {
	enter  Value
	closes *node
}

// fill is template with every text "$" replaced by shared, rebuilt bottom-up with an explicit stack.
func fill(template, shared Value) (Value, error) {
	todo := []buildFrame{{enter: template}}
	var done []Value
	for len(todo) > 0 {
		f := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		if c := f.closes; c != nil {
			count := len(c.items)
			if c.kind == KindMap {
				count = len(c.keys)
			}
			parts := slices.Clone(done[len(done)-count:])
			done = done[:len(done)-count]
			var v Value
			var err error
			if c.kind == KindMap {
				v, err = mapOfParts(c.keys, parts)
			} else {
				v, err = arrayOf(parts)
			}
			if err != nil {
				return Value{}, err
			}
			done = append(done, v)
			continue
		}
		v := f.enter
		switch v.Kind() {
		case KindText:
			if v.n.text == "$" {
				done = append(done, shared)
				continue
			}
		case KindArray, KindMap:
			todo = append(todo, buildFrame{closes: v.n})
			parts := v.n.items
			if v.Kind() == KindMap {
				parts = v.n.vals
			}
			for i := len(parts) - 1; i >= 0; i-- {
				todo = append(todo, buildFrame{enter: parts[i]})
			}
			continue
		}
		done = append(done, v)
	}
	if len(done) != 1 {
		return Value{}, typeMismatch("an empty template")
	}
	return done[0], nil
}

// mapOfParts is the map of keys (a map's, in canonical order) to vals, dropping null values.
func mapOfParts(keys []string, vals []Value) (Value, error) {
	ks := make([]string, 0, len(keys))
	vs := make([]Value, 0, len(vals))
	for i, k := range keys {
		if !vals[i].IsNull() {
			ks, vs = append(ks, k), append(vs, vals[i])
		}
	}
	return mapOfSorted(ks, vs)
}
