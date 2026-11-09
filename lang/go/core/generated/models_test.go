package generated_test

import (
	"errors"
	"fmt"
	"math/big"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"github.com/scasplte2/dilf4s/lang/go/core"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/canonicalvectors"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/crypto"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/jlvm"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/jlvmvectors"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/kesvectors"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/merkle"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/merklevectors"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/refusalvectors"
	"github.com/scasplte2/dilf4s/lang/go/core/generated/trievectors"
)

// vectorsDir is the repository's shared/vectors.
const vectorsDir = "../../../../shared/vectors"

func readVectors(t testing.TB, file string) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(vectorsDir, file))
	if err != nil {
		t.Fatalf("%s: %v", file, err)
	}
	return string(b)
}

// schemas is every vector file and the rule of its schema (shared/schemas/<name>-vectors.cddl).
func schemas() map[string]string {
	return map[string]string{
		"canonical_encoding_v1.json": "canonical-vectors:canonical-vectors",
		"diagnostics_v1.json":        "diagnostic-vectors:diagnostic-vectors",
		"jlvm_v1.json":               "jlvm-vectors:jlvm-vectors",
		"kes_v1.json":                "kes-vectors:kes-vectors",
		"merkle_v1.json":             "merkle-vectors:merkle-vectors",
		"poseidon_v1.json":           "poseidon-vectors:poseidon-vectors",
		"refusals_v1.json":           "refusal-vectors:refusal-vectors",
		"sigma_dilf4s_v1.json":       "sigma-vectors:sigma-vectors",
		"sigma_refusals_v1.json":     "sigma-refusal-vectors:sigma-refusal-vectors",
		"sigma_v1.json":              "sigma-vectors:sigma-vectors",
		"signing_v1.json":            "signing-vectors:signing-vectors",
		"trie_v1.json":               "trie-vectors:trie-vectors",
		"versioned_trie_v1.json":     "versioned-trie-vectors:versioned-trie-vectors",
		"vrf_v1.json":                "vrf-vectors:vrf-vectors",
		"zk_jlvm_v1.json":            "zk-jlvm-vectors:zk-jlvm-vectors",
		"zk_v1.json":                 "zk-vectors:zk-vectors",
	}
}

func codecOf(t testing.TB, rule string) modelCodec {
	t.Helper()
	i := slices.IndexFunc(models(), func(m modelCodec) bool { return m.rule == rule })
	if i < 0 {
		t.Fatalf("no model %s", rule)
	}
	return models()[i]
}

func render(t testing.TB, v core.Value) string {
	t.Helper()
	s, err := core.RenderJSONWithin(v, core.MaxRenderBytes)
	if err != nil {
		t.Fatalf("renders: %v", err)
	}
	return s
}

func TestEveryVectorFileHasASchema(t *testing.T) {
	entries, err := os.ReadDir(vectorsDir)
	if err != nil {
		t.Fatal(err)
	}
	var present []string
	for _, e := range entries {
		if strings.HasSuffix(e.Name(), ".json") {
			present = append(present, e.Name())
		}
	}
	var known []string
	for f := range schemas() {
		known = append(known, f)
	}
	slices.Sort(known)
	if !slices.Equal(present, known) {
		t.Fatalf("vector files %v, schemas for %v", present, known)
	}
}

// Every vector file decodes into the generated type of its schema and encodes back to the same value: nothing in a
// vector file escapes its schema. This exercises the emitter on records, unions with text alternatives, literals,
// ranges and recursion.
func TestEveryVectorFileRoundTripsThroughItsType(t *testing.T) {
	for file, rule := range schemas() {
		t.Run(file, func(t *testing.T) {
			value, err := core.ParseJSON(readVectors(t, file))
			if err != nil {
				t.Fatalf("not canonical JSON: %v", err)
			}
			again, err := codecOf(t, rule).roundTrip(value)
			if err != nil {
				t.Fatalf("does not fit its schema: %v", err)
			}
			if !again.Equal(value) {
				t.Fatalf("changes on a round trip")
			}
			if render(t, again) != render(t, value) {
				t.Fatalf("renders differently after a round trip")
			}
		})
	}
}

func TestASchemaViolationNamesTheFailingPath(t *testing.T) {
	bad := regexp.MustCompile(`"leaves"\s*:\s*\[\s*"`).ReplaceAllLiteralString(readVectors(t, "merkle_v1.json"), `"leaves": ["zz`)
	if bad == readVectors(t, "merkle_v1.json") {
		t.Fatal("the vector file changed shape: no leaves to break")
	}
	value, err := core.ParseJSON(bad)
	if err != nil {
		t.Fatal(err)
	}
	_, err = merklevectors.DecodeMerkleVectors(value)
	var failure core.DecodeFailure
	if !errors.As(err, &failure) {
		t.Fatalf("not a DecodeFailure: %v", err)
	}
	path := failure.Path()
	if len(path) != 2 || path[0].Head() != "leaves" || path[1].Head() != "[0]" {
		t.Fatalf("path %v", path)
	}
	if !strings.HasPrefix(failure.Error(), "leaves.[0]: ") {
		t.Fatalf("message %q", failure.Error())
	}
}

func TestUnionsAreToldApartByTypeWhenEncodingAndByKeysWhenDecoding(t *testing.T) {
	present := trievectors.TriePresent{Present: "00ff"}
	v, err := trievectors.EncodeTrieLookupResult(present, core.Encoding{})
	if err != nil {
		t.Fatal(err)
	}
	if got := render(t, v); got != `{"present":"00ff"}` {
		t.Fatalf("record alternative renders as %s", got)
	}
	back, err := trievectors.DecodeTrieLookupResult(v)
	if err != nil || back != trievectors.TrieLookupResult(present) {
		t.Fatalf("decodes as %v, %v", back, err)
	}
	absent, err := trievectors.EncodeTrieLookupResult(trievectors.TrieLookupResultAbsent{}, core.Encoding{})
	if err != nil || render(t, absent) != `"absent"` {
		t.Fatalf("text alternative: %v, %v", absent, err)
	}
	notCovered, _ := core.Text("notCovered")
	if r, err := trievectors.DecodeTrieLookupResult(notCovered); err != nil || r != trievectors.TrieLookupResult(trievectors.TrieLookupResultNotCovered{}) {
		t.Fatalf("notCovered decodes as %v, %v", r, err)
	}
	other, _ := core.Text("other")
	if _, err := trievectors.DecodeTrieLookupResult(other); err == nil {
		t.Fatal("another text decoded")
	}
	// a nil choice, and a pointer to an alternative, are no alternative
	for _, x := range []trievectors.TrieLookupResult{nil, &present} {
		_, err := trievectors.EncodeTrieLookupResult(x, core.Encoding{})
		var mismatch core.CanonicalErrorTypeMismatch
		if !errors.As(err, &mismatch) {
			t.Fatalf("%T encodes: %v", x, err)
		}
	}
}

func TestAnEncoderRefusesWhatItsDecoderWouldReject(t *testing.T) {
	inclusion := merkle.MerkleInclusion{Index: 1 << 63, Size: 1}
	if _, err := merkle.EncodeMerkleInclusion(inclusion, core.Encoding{}); err == nil {
		t.Fatal("tree indices stop at 2^63 - 1")
	}
	digest := canonicalvectors.CanonicalDigest{Algorithm: "SHA-256", Message: "0A"}
	if _, err := canonicalvectors.EncodeCanonicalDigest(digest, core.Encoding{}); err == nil {
		t.Fatal("hex is lowercase")
	}
	if _, err := crypto.DecodeEd25519Signature(core.Bytes(make([]byte, 63))); err == nil {
		t.Fatal("a 63-byte signature decoded")
	}
	// a Go string type admits any string by conversion, and an integer type any integer: the encoders check
	if _, err := jlvm.EncodeJlvmOp(jlvm.JlvmOp("no-such-op"), core.Encoding{}); err == nil {
		t.Fatal("an operation that is not one encoded")
	}
	if _, err := kesvectors.EncodeKesCheck("", core.Encoding{}); err == nil {
		t.Fatal("the zero text member encoded")
	}
}

func TestCDDLIntegersFollowRFC8610(t *testing.T) {
	bits := func(n string) core.Value {
		i, ok := new(big.Int).SetString(n, 10)
		if !ok {
			t.Fatal(n)
		}
		iv, err := core.Integer(i)
		if err != nil {
			t.Fatal(err)
		}
		empty, _ := core.Text("")
		m, err := core.Map(core.Entry{Key: "bits", Value: iv}, core.Entry{Key: "bytes", Value: empty})
		if err != nil {
			t.Fatal(err)
		}
		return m
	}
	if _, err := trievectors.DecodeTrieBits(bits("18446744073709551615")); err != nil {
		t.Fatalf("2^64 - 1 is a uint: %v", err)
	}
	for _, n := range []string{"18446744073709551616", "-1"} {
		if _, err := trievectors.DecodeTrieBits(bits(n)); err == nil {
			t.Fatalf("%s is not a uint", n)
		}
	}
}

func TestARecursiveModelRoundTripsThroughCBOR(t *testing.T) {
	program := `{"jlvm":1,"expr":{"op":"+","args":[{"lit":1},{"var":"x","default":{"path":{"lit":"y"}}},{"map":{"k":{}}}]}}`
	parsed, err := core.FromJSON(program, jlvm.DecodeJlvmProgram)
	if err != nil {
		t.Fatal(err)
	}
	b, err := core.ToCBOR(parsed, jlvm.EncodeJlvmProgram)
	if err != nil {
		t.Fatal(err)
	}
	again, err := core.FromCBOR(b, jlvm.DecodeJlvmProgram)
	if err != nil {
		t.Fatal(err)
	}
	b2, err := core.ToCBOR(again, jlvm.EncodeJlvmProgram)
	if err != nil || string(b) != string(b2) {
		t.Fatalf("a second round trip differs: %v", err)
	}
	value, _ := core.ParseJSON(program)
	if v, _ := jlvm.EncodeJlvmProgram(parsed, core.Encoding{}); render(t, v) != render(t, value) {
		t.Fatal("the program changes on a round trip")
	}
}

// A Go value nests without bound; encoding one past the depth limit is a typed DepthExceeded, refused before
// descending, so its recursion stops at MaxDepth levels.
func TestEncodingAModelNestedPastTheDepthLimitIsATypedError(t *testing.T) {
	var expr jlvm.JlvmExpr = jlvm.JlvmNull{}
	for range 20_000 {
		expr = jlvm.JlvmVarAt{Path: expr}
	}
	_, err := jlvm.EncodeJlvmExpr(expr, core.Encoding{})
	var deep core.CanonicalErrorDepthExceeded
	if !errors.As(err, &deep) || deep.Limit != core.MaxDepth {
		t.Fatalf("encodes: %v", err)
	}
}

// A Go model shares its parts: [e, e] doubled 60 times takes 60 slices, and stands for a value of 2^60 nodes (the
// fixtures fork a record the same way, through two fields). Encoding charges each node it builds against the budget,
// so it is refused as TooLarge once the budget is built, never expanded: within a small budget, after building at
// most that much; within the default, its limit.
func TestEncodingASharedModelIsBoundedByTheBudget(t *testing.T) {
	var doubled, applied jlvm.JlvmExpr = jlvm.JlvmNull{}, jlvm.JlvmNull{}
	for range 60 {
		doubled = jlvm.JlvmArray{Arr: []jlvm.JlvmExpr{doubled, doubled}}
		applied = jlvm.JlvmApply{Op: jlvm.JlvmOpAdd, Args: []jlvm.JlvmExpr{applied, applied, applied}}
	}
	for name, x := range map[string]jlvm.JlvmExpr{"a doubled array": doubled, "a tripled application": applied} {
		var tooLarge core.TooLarge
		if _, err := jlvm.EncodeJlvmExpr(x, core.EncodingWithin(10_000)); !errors.As(err, &tooLarge) || tooLarge.Limit != 10_000 {
			t.Errorf("%s within 10,000: %v", name, err)
		}
		if _, err := core.ToCBORWithin(x, jlvm.EncodeJlvmExpr, 1000); !errors.As(err, &tooLarge) || tooLarge.Limit != 1000 {
			t.Errorf("%s as CBOR within 1,000: %v", name, err)
		}
	}
	// the default budget names its limit; a byte string over it is refused by its length, before it is copied
	if _, err := jlvm.EncodeJlvmLit(jlvm.JlvmLit{Lit: core.Bytes(make([]byte, core.DefaultEncodingWeight))}, core.Encoding{}); err == nil {
		t.Error("a literal over the default budget encoded")
	}
	var tooLarge core.TooLarge
	if _, err := core.ToCBOR(make([]byte, core.DefaultEncodingWeight), core.EncodeBytes); !errors.As(err, &tooLarge) || tooLarge.Limit != core.DefaultEncodingWeight {
		t.Errorf("bytes over the default budget: %v", err)
	}
	// an encoding within its budget is the whole value, and an Encoding root is reusable: each encode counts its own
	small := jlvm.JlvmArray{Arr: []jlvm.JlvmExpr{jlvm.JlvmNull{}, jlvm.JlvmNull{}}}
	root := core.EncodingWithin(8)
	for range 3 {
		if v, err := jlvm.EncodeJlvmExpr(small, root); err != nil || v.Weight() != 8 {
			t.Fatalf("within its budget: %v, %v", v, err)
		}
	}
}

// Decoding copies every byte string out of the value, and encoding copies every one in: no model shares memory with
// a value or with its caller.
func TestModelsShareNoBytesWithValues(t *testing.T) {
	path := []byte{1, 2, 3}
	inclusion := merkle.MerkleInclusion{Index: 0, Size: 1, Path: [][]byte{path}}
	v, err := merkle.EncodeMerkleInclusion(inclusion, core.Encoding{})
	if err != nil {
		t.Fatal(err)
	}
	path[0] = 9
	decoded, err := merkle.DecodeMerkleInclusion(v)
	if err != nil {
		t.Fatal(err)
	}
	if decoded.Path[0][0] != 1 {
		t.Fatal("the value shares the caller's bytes")
	}
	decoded.Path[0][1] = 9
	again, _ := merkle.DecodeMerkleInclusion(v)
	if again.Path[0][1] != 2 {
		t.Fatal("a decoded model shares the value's bytes")
	}
}

// recipe is a built value's leaf and steps, whichever vector schema holds it.
type recipe struct {
	leaf  string
	steps []core.BuildStep
}

func buildRecipe(t *testing.T, name string, r recipe) core.Value {
	t.Helper()
	leaf, err := core.FromHex(r.leaf)
	if err != nil {
		t.Fatal(err)
	}
	v, err := core.Build(leaf, r.steps)
	if err != nil {
		t.Fatalf("%s does not build: %v", name, err)
	}
	if v.Depth() > core.MaxDepth || v.Weight() < 1 {
		t.Fatalf("%s: depth %d, weight %d", name, v.Depth(), v.Weight())
	}
	return v
}

func step(t *testing.T, template string, times uint64) core.BuildStep {
	t.Helper()
	b, err := core.FromHex(template)
	if err != nil {
		t.Fatal(err)
	}
	return core.BuildStep{Template: b, Times: times}
}

func refusalRecipe(t *testing.T, b refusalvectors.BuiltValue) recipe {
	r := recipe{leaf: b.Leaf}
	for _, s := range b.Steps.OrElse(nil) {
		switch x := s.(type) {
		case refusalvectors.BuiltWrap:
			r.steps = append(r.steps, step(t, x.Template, x.Times))
		case refusalvectors.BuiltCopies:
			r.steps = append(r.steps, core.BuildStep{Copies: core.Some(x.Copies)})
		}
	}
	return r
}

func jlvmRecipe(t *testing.T, b jlvmvectors.BuiltValue) recipe {
	r := recipe{leaf: b.Leaf}
	for _, s := range b.Steps.OrElse(nil) {
		switch x := s.(type) {
		case jlvmvectors.BuiltWrap:
			r.steps = append(r.steps, step(t, x.Template, x.Times))
		case jlvmvectors.BuiltCopies:
			r.steps = append(r.steps, core.BuildStep{Copies: core.Some(x.Copies)})
		}
	}
	return r
}

// The core rows of refusals_v1.json and jlvm_v1.json: every built value (conformance.md section 6) builds with core's
// one builder, with sharing, within the depth limit, its weight read from its cache, never by expanding it. The
// verifier checks that refuse them are the proofs' and the JLVM's.
func TestEveryBuiltValueBuilds(t *testing.T) {
	value, err := core.ParseJSON(readVectors(t, "refusals_v1.json"))
	if err != nil {
		t.Fatal(err)
	}
	refusals, err := refusalvectors.DecodeRefusalVectors(value)
	if err != nil {
		t.Fatal(err)
	}
	built, heaviest := 0, uint64(0)
	for _, c := range refusals.Cases {
		var inputs []refusalvectors.BuiltValue
		if in, ok := c.Input.Get(); ok {
			inputs = append(inputs, in)
		}
		inputs = append(inputs, c.Inputs.OrElse(nil)...)
		if claim, ok := c.Claim.Get(); ok {
			inputs = append(inputs, claim)
		}
		for _, e := range c.Entries.OrElse(nil) {
			inputs = append(inputs, e.Value)
		}
		for _, u := range c.Batch.OrElse(nil) {
			inputs = append(inputs, u.Value)
		}
		for _, in := range inputs {
			heaviest = max(heaviest, buildRecipe(t, c.Name, refusalRecipe(t, in)).Weight())
			built++
		}
	}
	value, err = core.ParseJSON(readVectors(t, "jlvm_v1.json"))
	if err != nil {
		t.Fatal(err)
	}
	jlvmCases, err := jlvmvectors.DecodeJlvmVectors(value)
	if err != nil {
		t.Fatal(err)
	}
	for _, c := range jlvmCases.Cases {
		bindings := c.Built.OrElse(core.TextMap[jlvmvectors.BuiltValue]{})
		for _, b := range bindings.All() {
			heaviest = max(heaviest, buildRecipe(t, c.Name, jlvmRecipe(t, b)).Weight())
			built++
		}
	}
	for _, d := range jlvmCases.Decodings {
		heaviest = max(heaviest, buildRecipe(t, d.Name, jlvmRecipe(t, d.Form)).Weight())
		built++
	}
	if built < 60 {
		t.Fatalf("only %d built values", built)
	}
	// some recipe describes a value far heavier than any encoding of it could be held
	if heaviest <= core.MaxEncodedBytes {
		t.Fatalf("the heaviest built value weighs %d", heaviest)
	}
}

// A model is refused at a byte limit for its value alone: a null default, which the record's map leaves out, is never
// charged to it, so the record encodes within the seven bytes its value takes either way.
func TestANullMemberIsRefusedForItsValueAlone(t *testing.T) {
	for _, v := range []jlvm.JlvmVar{{Var: "x"}, {Var: "x", Default: core.Some(core.Null())}} {
		got, err := core.ToCBORWithin(v, jlvm.EncodeJlvmVar, 7)
		if err != nil || fmt.Sprintf("%x", got) != "a1637661726178" {
			t.Errorf("%+v within 7 bytes: %x, %v", v, got, err)
		}
		if _, err := core.ToCBORWithin(v, jlvm.EncodeJlvmVar, 6); !errors.As(err, new(core.TooLarge)) {
			t.Errorf("%+v within 6 bytes: %v", v, err)
		}
	}
}
