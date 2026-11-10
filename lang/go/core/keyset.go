package core

import (
	"iter"
	"slices"
)

// keySet is a set of caller-chosen text keys, compared, never hashed (docs/guides/porting.md: a key a caller or a peer
// chooses must not reach a hash table): sorted runs of distinct power-of-two sizes, merged like a binary counter, as
// the TypeScript SDK's KeySet. Lookups are O(log^2 n) comparisons, and an insertion amortised O(log^2 n).
type keySet struct {
	runs [][]string
}

func (s *keySet) has(key string) bool {
	for _, run := range s.runs {
		if _, found := slices.BinarySearch(run, key); found {
			return true
		}
	}
	return false
}

// add adds key, or reports false (and changes nothing) when it is present.
func (s *keySet) add(key string) bool {
	if s.has(key) {
		return false
	}
	run := []string{key}
	for len(s.runs) > 0 && len(s.runs[len(s.runs)-1]) == len(run) {
		run = mergeRuns(s.runs[len(s.runs)-1], run)
		s.runs = s.runs[:len(s.runs)-1]
	}
	s.runs = append(s.runs, run)
	return true
}

func mergeRuns(a, b []string) []string {
	out := make([]string, 0, len(a)+len(b))
	i, j := 0, 0
	for i < len(a) && j < len(b) {
		if a[i] < b[j] {
			out = append(out, a[i])
			i++
		} else {
			out = append(out, b[j])
			j++
		}
	}
	return append(append(out, a[i:]...), b[j:]...)
}

// TextMap is an immutable map with text keys, in canonical key order (shorter UTF-8 first, then bytewise): a generated
// model's { * text => T }. Its keys are compared, never hashed, as a caller or a peer chooses them; lookups are binary
// searches. The zero TextMap is empty.
type TextMap[T any] struct {
	keys []string
	vals []T
}

// TextEntry is an entry of a TextMap.
type TextEntry[T any] struct {
	Key   string
	Value T
}

// NewTextMap is the map of entries, in any order: a repeated key is CanonicalErrorDuplicateKey, at its first repeat
// in the order given. Keys are checked as text when the map is encoded.
func NewTextMap[T any](entries ...TextEntry[T]) (TextMap[T], error) {
	var seen keySet
	for _, e := range entries {
		if !seen.add(e.Key) {
			return TextMap[T]{}, CanonicalErrorDuplicateKey{Key: PreviewOf(e.Key)}
		}
	}
	sorted := slices.Clone(entries)
	slices.SortFunc(sorted, func(a, b TextEntry[T]) int { return canonicalKeyOrder(a.Key, b.Key) })
	m := TextMap[T]{keys: make([]string, len(sorted)), vals: make([]T, len(sorted))}
	for i, e := range sorted {
		m.keys[i], m.vals[i] = e.Key, e.Value
	}
	return m, nil
}

// TextMapFrom is the map of the caller's own Go map m, in canonical key order (never m's order).
func TextMapFrom[T any](m map[string]T) TextMap[T] {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	slices.SortFunc(keys, canonicalKeyOrder)
	out := TextMap[T]{keys: keys, vals: make([]T, len(keys))}
	for i, k := range keys {
		out.vals[i] = m[k]
	}
	return out
}

// Len is the number of entries.
func (m TextMap[T]) Len() int { return len(m.keys) }

// Get is the value at key, if present.
func (m TextMap[T]) Get(key string) (T, bool) {
	i, found := slices.BinarySearchFunc(m.keys, key, canonicalKeyOrder)
	if !found {
		var zero T
		return zero, false
	}
	return m.vals[i], true
}

// Keys is a copy of the keys, in canonical key order.
func (m TextMap[T]) Keys() []string { return slices.Clone(m.keys) }

// All yields the entries in canonical key order.
func (m TextMap[T]) All() iter.Seq2[string, T] {
	return func(yield func(string, T) bool) {
		for i, k := range m.keys {
			if !yield(k, m.vals[i]) {
				return
			}
		}
	}
}
