package core

import (
	"slices"
	"strconv"
	"strings"
)

// DecodeFailure is why a canonical value is not a model's shape, and where: the path runs from the root (map keys and
// [i]). A key may be the caller's, so each segment is a Preview; the reason quotes caller input only through one
// (docs/specs/conformance.md section 8). The zero DecodeFailure is a failure at the root with an empty reason.
type DecodeFailure struct {
	path   []Preview
	reason string
}

// Failure is a decode failure at the value itself.
func Failure(reason string) DecodeFailure { return DecodeFailure{reason: reason} }

// Path is a copy of the path, from the root.
func (f DecodeFailure) Path() []Preview { return slices.Clone(f.path) }

// Reason is what is wrong there.
func (f DecodeFailure) Reason() string { return f.reason }

// At is f one level further down: segment is prepended to the path.
func (f DecodeFailure) At(segment string) DecodeFailure {
	path := make([]Preview, 0, len(f.path)+1)
	path = append(path, PreviewOf(segment))
	return DecodeFailure{path: append(path, f.path...), reason: f.reason}
}

// Error is the path from the root (<root> when empty), each segment in its short form, then the reason.
func (f DecodeFailure) Error() string {
	var b strings.Builder
	if len(f.path) == 0 {
		b.WriteString("<root>")
	}
	for i, p := range f.path {
		if i > 0 {
			b.WriteByte('.')
		}
		b.WriteString(p.Short())
	}
	b.WriteString(": ")
	b.WriteString(f.reason)
	return b.String()
}

// at places err, a decoder's refusal, under segment: a DecodeFailure gets the segment prepended; any other error (none
// of this package's decoders give one) is a DecodeFailure of its own there.
func at(err error, segment string) error {
	if f, ok := err.(DecodeFailure); ok {
		return f.At(segment)
	}
	return Failure(err.Error()).At(segment)
}

// atIndex is at for the array element i.
func atIndex(err error, i int) error { return at(err, "["+strconv.Itoa(i)+"]") }

// TooLarge is a value whose canonical encoding, commitment or JSON is longer than a limit, found without producing
// more than the limit (docs/specs/conformance.md section 4). It carries the limit as applied (a negative limit is 0,
// a limit past the ceiling is the ceiling), never a measured size.
type TooLarge struct {
	Limit int
}

func (t TooLarge) Error() string { return "the encoding is over " + strconv.Itoa(t.Limit) + " bytes" }

// typeMismatch is a value outside its schema, or an argument outside its type.
func typeMismatch(reason string) error { return CanonicalErrorTypeMismatch{Reason: reason} }

// MaxEncodedBytes is the longest encoding this SDK builds: 2^31 - 9 bytes, the reference's and the TypeScript SDK's
// ceiling (the JVM's largest array). Encode is EncodeWithin at it, and the content hash and value commitments refuse
// past it as TooLarge, so the Go SDK refuses exactly what the reference refuses (docs/specs/conformance.md section 4).
const MaxEncodedBytes = 1<<31 - 9
