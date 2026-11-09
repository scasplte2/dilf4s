package core_test

import (
	"errors"
	"math"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/scasplte2/dilf4s/sdk/go/core"
	"github.com/scasplte2/dilf4s/sdk/go/core/generated/canonicalvectors"
	"github.com/scasplte2/dilf4s/sdk/go/core/generated/diagnosticvectors"
	"github.com/scasplte2/dilf4s/sdk/go/core/generated/merklevectors"
	"github.com/scasplte2/dilf4s/sdk/go/core/generated/versionedtrie"
	"github.com/scasplte2/dilf4s/sdk/go/core/generated/versionedtrievectors"
)

// vectorsDir is the repository's shared/vectors.
const vectorsDir = "../../../shared/vectors"

func readVectors(t testing.TB, file string) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(vectorsDir, file))
	if err != nil {
		t.Fatalf("%s: %v", file, err)
	}
	return string(b)
}

// load is a vector file parsed with this SDK's JSON parser and decoded into its generated type.
func load[T any](t testing.TB, file string, dec func(core.Value) (T, error)) T {
	t.Helper()
	x, err := core.FromJSON(readVectors(t, file), dec)
	if err != nil {
		t.Fatalf("%s does not load: %v", file, err)
	}
	return x
}

func fromHex(t testing.TB, s string) []byte {
	t.Helper()
	b, err := core.FromHex(s)
	if err != nil {
		t.Fatalf("hex %q: %v", s, err)
	}
	return b
}

// row is a refusal as a vector row records it: its kind, and its normative limit when it has one (conformance.md
// section 3).
type row struct {
	kind  string
	limit core.Optional[uint64]
}

func refusalOf(err error) row {
	var ce core.CanonicalError
	if !errors.As(err, &ce) {
		return row{kind: "not a CanonicalError: " + err.Error()}
	}
	switch e := ce.(type) {
	case core.CanonicalErrorRationalTooWide:
		return row{kind: string(e.Kind()), limit: core.Some(e.Limit)}
	case core.CanonicalErrorIntegerTooWide:
		return row{kind: string(e.Kind()), limit: core.Some(e.Limit)}
	}
	return row{kind: string(ce.Kind())}
}

func canonical(t testing.TB) canonicalvectors.CanonicalVectors {
	return load(t, "canonical_encoding_v1.json", canonicalvectors.DecodeCanonicalVectors)
}

func knownAlgorithm(t testing.TB, name string) core.HashAlgorithm {
	t.Helper()
	a, err := core.ParseHashAlgorithm(name)
	if err != nil {
		t.Fatalf("%s: %v", name, err)
	}
	return a
}

func TestCanonicalJSONCases(t *testing.T) {
	vectors := canonical(t)
	tag, err := core.NewDomainTag(vectors.ContentHashTag)
	if err != nil {
		t.Fatal(err)
	}
	algorithm := knownAlgorithm(t, vectors.ContentHashAlgorithm)
	accepted, rejected := 0, 0
	for _, c := range vectors.Json {
		switch c := c.(type) {
		case canonicalvectors.CanonicalJsonAccepted:
			accepted++
			value, err := core.ParseJSON(c.Json)
			if err != nil {
				t.Errorf("%.60q: %v", c.Json, err)
				continue
			}
			b, err := core.Encode(value)
			if err != nil || hexOf(b) != c.Cbor {
				t.Errorf("%.60q: canonical CBOR %x, %v", c.Json, b, err)
			}
			h, err := core.ContentHash(algorithm, tag, value)
			if err != nil || hexOf(h) != c.ContentHash {
				t.Errorf("%.60q: content hash %x, %v", c.Json, h, err)
			}
			if r, err := core.RenderJSONWithin(value, core.MaxRenderBytes); err != nil || r != c.Render {
				t.Errorf("%.60q: renders as %.60q, %v", c.Json, r, err)
			}
			decoded, err := core.Decode(fromHex(t, c.Cbor))
			if err != nil || !decoded.Equal(value) {
				t.Errorf("%.60q: the CBOR decodes as %v, %v", c.Json, decoded, err)
			}
			if again, _ := core.Encode(decoded); hexOf(again) != c.Cbor {
				t.Errorf("%.60q: decode then encode is not the identity", c.Json)
			}
		case canonicalvectors.CanonicalJsonRejected:
			rejected++
			_, err := core.ParseJSON(c.Json)
			want := row{kind: string(c.Error), limit: c.Limit}
			if err == nil {
				t.Errorf("%.60q: accepted, want %v", c.Json, want)
			} else if got := refusalOf(err); got != want {
				t.Errorf("%.60q: %v, want %v", c.Json, got, want)
			}
		}
	}
	if accepted == 0 || rejected == 0 {
		t.Fatalf("%d accepted and %d rejected cases", accepted, rejected)
	}
}

func TestTheByteOrderMarkRowsReachTheParserWithTheirMark(t *testing.T) {
	var marked []string
	for _, c := range canonical(t).Json {
		if r, ok := c.(canonicalvectors.CanonicalJsonRejected); ok && len(r.Json) >= 3 && r.Json[:3] == "\xef\xbb\xbf" {
			marked = append(marked, r.Json)
			if r.Error != core.CanonicalErrorKindJsonSyntax {
				t.Errorf("%q is %s", r.Json, r.Error)
			}
		}
	}
	if !slices.Equal(marked, []string{"\xef\xbb\xbf1", "\xef\xbb\xbf{}"}) {
		t.Fatalf("marked rows %q", marked)
	}
}

func TestCanonicalModelCases(t *testing.T) {
	vectors := canonical(t)
	tag, _ := core.NewDomainTag(vectors.ContentHashTag)
	algorithm := knownAlgorithm(t, vectors.ContentHashAlgorithm)
	for _, c := range vectors.Model {
		value, err := core.Decode(fromHex(t, c.Cbor))
		if err != nil {
			t.Errorf("%s: %v", c.Value, err)
			continue
		}
		if b, _ := core.Encode(value); hexOf(b) != c.Cbor {
			t.Errorf("%s: re-encodes as %x", c.Value, b)
		}
		if h, err := core.ContentHash(algorithm, tag, value); err != nil || hexOf(h) != c.ContentHash {
			t.Errorf("%s: content hash %x, %v", c.Value, h, err)
		}
	}
}

func TestCanonicalLimitCases(t *testing.T) {
	for _, c := range canonical(t).Limits {
		value, err := core.Decode(fromHex(t, c.Value))
		if err != nil {
			t.Fatalf("%s: %v", c.Name, err)
		}
		limit := int(min(c.MaxBytes, math.MaxInt)) // a limit past int is past the ceiling, which it is then
		var output string
		if c.Encoding == canonicalvectors.CanonicalLimitEncodingCbor {
			var b []byte
			if b, err = core.EncodeWithin(value, limit); err == nil {
				output = hexOf(b)
			}
			if n, lerr := core.EncodedLengthWithin(value, limit); (lerr == nil) != (err == nil) || (err == nil && n != len(b)) {
				t.Errorf("%s within %d: the measured length %d, %v disagrees", c.Name, limit, n, lerr)
			}
		} else {
			output, err = core.RenderJSONWithin(value, limit)
		}
		var got canonicalvectors.CanonicalLimitOutcome = canonicalvectors.CanonicalLimitOutcomeAccepted{}
		var tooLarge core.TooLarge
		var ce core.CanonicalError
		switch {
		case err == nil:
		case errors.As(err, &tooLarge):
			got = canonicalvectors.CanonicalLimitRefusal{Kind: canonicalvectors.CanonicalLimitKindTooLarge{}, Limit: core.Some(uint64(tooLarge.Limit))}
		case errors.As(err, &ce):
			got = canonicalvectors.CanonicalLimitRefusal{Kind: canonicalvectors.CanonicalLimitKindCanonicalErrorKind{Value: ce.Kind()}}
		default:
			t.Fatalf("%s: %v", c.Name, err)
		}
		if got != c.Outcome {
			t.Errorf("%s within %d: %v, want %v", c.Name, limit, got, c.Outcome)
		}
		want, accepted := c.Output.Get()
		if accepted != (err == nil) || (accepted && output != want) {
			t.Errorf("%s within %d: output %q, want %q", c.Name, limit, output, want)
		}
	}
}

func TestCanonicalRenderAndCBORRejections(t *testing.T) {
	vectors := canonical(t)
	for _, c := range vectors.RenderRejections {
		value, err := core.Decode(fromHex(t, c.Cbor))
		if err != nil {
			t.Fatalf("%.40s: %v", c.Cbor, err)
		}
		_, err = core.RenderJSONWithin(value, core.MaxRenderBytes)
		if err == nil {
			t.Errorf("%.40s rendered", c.Cbor)
		} else if got := refusalOf(err); got.kind != string(c.Error) {
			t.Errorf("%.40s: %v, want %s", c.Cbor, got, c.Error)
		}
	}
	for _, c := range vectors.CborRejections {
		_, err := core.Decode(fromHex(t, c.Cbor))
		want := row{kind: string(c.Error), limit: c.Limit}
		if err == nil {
			t.Errorf("%.40s accepted, want %v", c.Cbor, want)
		} else if got := refusalOf(err); got != want {
			t.Errorf("%.40s: %v, want %v", c.Cbor, got, want)
		}
	}
}

// Coverage (conformance.md section 6): every CanonicalError kind has a row, AllCanonicalErrorKind listing the
// generated kinds.
func TestEveryCanonicalErrorKindHasARow(t *testing.T) {
	vectors := canonical(t)
	stateKeys := load(t, "versioned_trie_v1.json", versionedtrievectors.DecodeVersionedTrieVectors)
	if missing := uncoveredCanonical(vectors, stateKeys); len(missing) > 0 {
		t.Errorf("no row for %v", missing)
	}
	// the gate fails on a planted gap: every row of one kind removed
	planted := vectors
	planted.CborRejections = slices.DeleteFunc(slices.Clone(planted.CborRejections), func(c canonicalvectors.CanonicalCborRejection) bool {
		return c.Error == core.CanonicalErrorKindInvalidUtf8
	})
	if missing := uncoveredCanonical(planted, stateKeys); !slices.Equal(missing, []core.CanonicalErrorKind{core.CanonicalErrorKindInvalidUtf8}) {
		t.Errorf("a planted gap is reported as %v", missing)
	}
}

// uncoveredCanonical is every kind of CanonicalError (over the generated kind list) that no row pins.
func uncoveredCanonical(vectors canonicalvectors.CanonicalVectors, stateKeys versionedtrievectors.VersionedTrieVectors) []core.CanonicalErrorKind {
	var pinned []core.CanonicalErrorKind
	for _, c := range vectors.Json {
		if r, ok := c.(canonicalvectors.CanonicalJsonRejected); ok {
			pinned = append(pinned, r.Error)
		}
	}
	for _, c := range vectors.RenderRejections {
		pinned = append(pinned, c.Error)
	}
	for _, c := range vectors.CborRejections {
		pinned = append(pinned, c.Error)
	}
	for _, c := range stateKeys.StateKeys {
		if c.Schema == versionedtrievectors.VtSchemaOutcomeTypeMismatch {
			pinned = append(pinned, core.CanonicalErrorKindTypeMismatch)
		}
	}
	var missing []core.CanonicalErrorKind
	for _, k := range core.AllCanonicalErrorKind() {
		if !slices.Contains(pinned, k) {
			missing = append(missing, k)
		}
	}
	return missing
}

func TestTaggedHashesAndDigests(t *testing.T) {
	vectors := canonical(t)
	for _, c := range vectors.TaggedHash {
		tag, err := core.NewDomainTag(c.Tag)
		if err != nil {
			t.Fatalf("%s: %v", c.Tag, err)
		}
		if h, err := core.TaggedHashOf(knownAlgorithm(t, c.Algorithm), tag, fromHex(t, c.Message)); err != nil || hexOf(h) != c.Digest {
			t.Errorf("tagged %s of %s: %x, %v", c.Algorithm, c.Message, h, err)
		}
	}
	for _, c := range vectors.Digests {
		if d, err := core.Digest(knownAlgorithm(t, c.Algorithm), fromHex(t, c.Message)); err != nil || hexOf(d) != c.Digest {
			t.Errorf("%s of %d bytes: %x, %v", c.Algorithm, len(c.Message)/2, d, err)
		}
	}
}

// hashOutcome is a hash call's outcome as a row records it: accepted, or the kind.
func hashOutcome(t testing.TB, err error) canonicalvectors.HashOutcome {
	t.Helper()
	if err == nil {
		return canonicalvectors.HashOutcomeAccepted{}
	}
	var he core.HashError
	if !errors.As(err, &he) {
		t.Fatalf("not a HashError: %v", err)
	}
	return canonicalvectors.HashOutcomeHashErrorKind{Value: he.Kind()}
}

func TestHashOutcomes(t *testing.T) {
	vectors := canonical(t)
	for _, c := range vectors.DomainTags {
		_, err := core.NewDomainTag(c.Tag)
		if got := hashOutcome(t, err); got != c.Outcome {
			t.Errorf("domain tag %q: %v, want %v", c.Tag, got, c.Outcome)
		}
	}
	for _, c := range vectors.Algorithms {
		a, err := core.ParseHashAlgorithm(c.Name)
		if got := hashOutcome(t, err); got != c.Outcome {
			t.Errorf("hash function name %q: %v, want %v", c.Name, got, c.Outcome)
		}
		if err == nil && string(a) != c.Name {
			t.Errorf("%q parses as %s", c.Name, a)
		}
	}
	for _, c := range vectors.DigestHex {
		d, err := core.DigestFromHex(knownAlgorithm(t, c.Algorithm), c.Hex)
		if got := hashOutcome(t, err); got != c.Outcome {
			t.Errorf("%s digest from hex %.40q: %v, want %v", c.Algorithm, c.Hex, got, c.Outcome)
		}
		var invalid core.HashErrorInvalidHex
		offset := core.None[uint64]()
		if errors.As(err, &invalid) {
			offset = core.Some(invalid.Offset)
		}
		if offset != c.Offset {
			t.Errorf("%s digest from hex %.40q: offset %v, want %v", c.Algorithm, c.Hex, offset, c.Offset)
		}
		if err == nil {
			want := c.Hex
			if len(want) >= 2 && want[:2] == "0x" {
				want = want[2:]
			}
			if hexOf(d) != toLower(want) {
				t.Errorf("%s digest from hex %q: %x", c.Algorithm, c.Hex, d)
			}
		}
	}
	for _, c := range vectors.Expansions {
		out, err := core.ExpandMessageXmd(knownAlgorithm(t, c.Algorithm), [][]byte{fromHex(t, c.Message)}, fromHex(t, c.Dst), c.Length)
		if got := hashOutcome(t, err); got != c.Outcome {
			t.Errorf("%s expand_message_xmd to %d: %v, want %v", c.Algorithm, c.Length, got, c.Outcome)
		}
		var invalid core.HashErrorInvalidExpansion
		refusal := core.None[core.ExpansionRefusal]()
		if errors.As(err, &invalid) {
			refusal = core.Some(invalid.Refusal)
		}
		if refusal != c.Refusal {
			t.Errorf("%s expand_message_xmd to %d: refusal %v, want %v", c.Algorithm, c.Length, refusal, c.Refusal)
		}
		want, accepted := c.Output.Get()
		if accepted != (err == nil) || (accepted && hexOf(out) != want) {
			t.Errorf("%s expand_message_xmd to %d: output %x, want %s", c.Algorithm, c.Length, out, want)
		}
	}
}

func toLower(s string) string {
	b := []byte(s)
	for i, c := range b {
		if c >= 'A' && c <= 'Z' {
			b[i] = c + 'a' - 'A'
		}
	}
	return string(b)
}

// Coverage (conformance.md section 6): every HashError kind has a row, and every expansion refusal.
func TestEveryHashErrorKindHasARow(t *testing.T) {
	vectors := canonical(t)
	pinned := map[core.HashErrorKind]bool{}
	note := func(o canonicalvectors.HashOutcome) {
		if k, ok := o.(canonicalvectors.HashOutcomeHashErrorKind); ok {
			pinned[k.Value] = true
		}
	}
	for _, c := range vectors.DomainTags {
		note(c.Outcome)
	}
	for _, c := range vectors.Algorithms {
		note(c.Outcome)
	}
	for _, c := range vectors.DigestHex {
		note(c.Outcome)
	}
	refused := map[core.ExpansionRefusal]bool{}
	for _, c := range vectors.Expansions {
		note(c.Outcome)
		if r, ok := c.Refusal.Get(); ok {
			refused[r] = true
		}
	}
	for _, k := range core.AllHashErrorKind() {
		if !pinned[k] {
			t.Errorf("no row for %s", k)
		}
	}
	for _, r := range core.AllExpansionRefusal() {
		if !refused[r] {
			t.Errorf("no row for the expansion refusal %s", r)
		}
	}
}

// versioned_trie_v1.json's state keys: the schema pattern decides, and the encoder agrees, refusing as TypeMismatch.
func TestStateKeysMatchTheSchemaPattern(t *testing.T) {
	vectors := load(t, "versioned_trie_v1.json", versionedtrievectors.DecodeVersionedTrieVectors)
	for _, c := range vectors.StateKeys {
		text, err := core.Text(c.Key)
		if err != nil {
			t.Fatalf("%.40q: %v", c.Key, err)
		}
		_, derr := versionedtrie.DecodeStateKey(text)
		if (derr == nil) != (c.Schema == versionedtrievectors.VtSchemaOutcomeAccepted) {
			t.Errorf("%.40q decodes: %v, want %s", c.Key, derr, c.Schema)
		}
		_, eerr := versionedtrie.EncodeStateKey(c.Key, core.Encoding{})
		got := versionedtrievectors.VtSchemaOutcomeAccepted
		if eerr != nil {
			got = versionedtrievectors.VtSchemaOutcome(refusalOf(eerr).kind)
		}
		if got != c.Schema {
			t.Errorf("%.40q encodes: %s, want %s", c.Key, got, c.Schema)
		}
	}
}

// merkle_v1.json's value commitments, built and streamed alike.
func TestValueCommitments(t *testing.T) {
	vectors := load(t, "merkle_v1.json", merklevectors.DecodeMerkleVectors)
	leafTag, _ := core.NewDomainTag("dilf4s/merkle/leaf/v1")
	leaf, _ := core.NewTaggedHash(core.SHA256, leafTag)
	for _, c := range vectors.ValueCommitments {
		value, err := core.Decode(fromHex(t, c.Cbor))
		if err != nil {
			t.Fatalf("%s: %v", c.Cbor, err)
		}
		commitment, err := core.ValueCommitment(value)
		if err != nil || hexOf(commitment) != c.Commitment {
			t.Errorf("%s commits as %x, %v", c.Cbor, commitment, err)
		}
		if h, err := leaf.Hash(commitment); err != nil || hexOf(h) != c.LeafHash {
			t.Errorf("%s: leaf hash %x, %v", c.Cbor, h, err)
		}
		tagHash, _ := core.Digest(core.SHA256, []byte("dilf4s/merkle/leaf/v1"))
		if h, err := core.HashCommitment(core.SHA256, [][]byte{tagHash, tagHash}, value); err != nil || hexOf(h) != c.LeafHash {
			t.Errorf("%s: streamed leaf hash %x, %v", c.Cbor, h, err)
		}
	}
}

// diagnostics_v1.json (conformance.md section 8): each repeated member name is refused as DuplicateKey with this
// preview.
func TestDiagnosticPreviews(t *testing.T) {
	vectors := load(t, "diagnostics_v1.json", diagnosticvectors.DecodeDiagnosticVectors)
	if len(vectors.Previews) == 0 {
		t.Fatal("no previews")
	}
	for _, c := range vectors.Previews {
		_, err := core.ParseJSON(c.Json)
		var dup core.CanonicalErrorDuplicateKey
		if !errors.As(err, &dup) {
			t.Errorf("%.40q: %v", c.Json, err)
			continue
		}
		p := dup.Key
		if p.Head() != c.Head || uint64(p.Length()) != c.Length || p.Truncated() != c.Truncated {
			t.Errorf("%.40q: preview %q %d %v, want %q %d %v", c.Json, p.Head(), p.Length(), p.Truncated(), c.Head, c.Length, c.Truncated)
		}
	}
}

// hexOf is the lowercase hex of bytes a test knows are short.
func hexOf(b []byte) string {
	s, _ := core.ToHex(b)
	return s
}
