package core

import (
	"math/big"
	"slices"
	"strings"
	"unicode/utf8"
)

// DefaultMaxScale is the largest accepted |fraction digits - exponent| of a JSON number, bounding the 10^scale it
// materialises.
const DefaultMaxScale = CanonicalMaxScale

// MaxDigits is the most significant digits a JSON number's significand (integer and fraction digits, leading zeros
// aside) or exponent may have, read or written: decimal conversion is superlinear, so the bound is checked on the text
// before any conversion.
const MaxDigits = CanonicalMaxDigits

// MaxRenderBytes is the longest JSON rendering, in UTF-8 bytes: 2^29 - 24, the reference's and the TypeScript SDK's
// ceiling (V8's longest string). A limit past it is it.
const MaxRenderBytes = 1<<29 - 24

// ParseJSON parses JSON text into a canonical value (docs/specs/canonical-encoding.md section 3), with the default
// scale bound. Numbers are read exactly from their text, never through a float; duplicate member names and unpaired
// surrogates are refused; nesting past MaxDepth is a typed error, never a stack overflow (the parser keeps its own
// stack). A refusal is the CanonicalError kind of the first fault in reading order; its offsets count UTF-8 bytes, as
// the Rust SDK's do.
//
// A Go string may hold bytes that are not UTF-8, which JSON text never does (RFC 8259 section 8.1): such text is
// refused as CanonicalErrorInvalidUtf8 at the first ill-formed byte, before it is parsed.
func ParseJSON(text string) (Value, error) { return ParseJSONWithMaxScale(text, DefaultMaxScale) }

// ParseJSONWithMaxScale is ParseJSON, refusing numbers whose scale exceeds maxScale. The bound may be lowered, never
// raised: the spec accepts no JSON number of a scale past DefaultMaxScale, so a maxScale outside 0..DefaultMaxScale is
// a CanonicalErrorTypeMismatch, before anything is read.
func ParseJSONWithMaxScale(text string, maxScale int) (Value, error) {
	if maxScale < 0 || maxScale > DefaultMaxScale {
		return Value{}, typeMismatch("the scale bound is not an integer from 0 to 10000")
	}
	if at, ok := invalidUTF8(text); ok {
		return Value{}, CanonicalErrorInvalidUtf8{Offset: uint64(at)}
	}
	p := parser{text: text, maxScale: maxScale}
	return p.run()
}

func syntax(offset int, reason JsonSyntaxReason) error {
	return CanonicalErrorJsonSyntax{Offset: uint64(offset), Reason: reason}
}

type jsonState uint8

const (
	expectValue jsonState = iota
	expectKey
	deliverValue
)

// jsonFrame is an open array or object. A member name is kept as generalised UTF-8 (WTF-8): \u escapes may leave an
// unpaired surrogate, which is only an error once its object is complete, as in the reference parser.
type jsonFrame struct {
	isObject bool
	items    []Value
	names    []string
	vals     []Value
	seen     *keySet
	key      string
}

type parser struct {
	text     string
	maxScale int
}

func (p *parser) at(i int) (byte, bool) {
	if i < 0 || i >= len(p.text) {
		return 0, false
	}
	return p.text[i], true
}

func (p *parser) skipWS(i int) int {
	for i < len(p.text) {
		switch p.text[i] {
		case ' ', '\t', '\n', '\r':
			i++
		default:
			return i
		}
	}
	return i
}

func (p *parser) digits(i int) int {
	for i < len(p.text) && p.text[i] >= '0' && p.text[i] <= '9' {
		i++
	}
	return i
}

func (p *parser) run() (Value, error) {
	var stack []jsonFrame
	pos, state := 0, expectValue
	var v Value
	for {
		var err error
		switch state {
		case expectValue:
			state, v, pos, err = p.expectValue(&stack, pos)
		case expectKey:
			state, pos, err = p.expectKey(stack, pos)
		default:
			var done bool
			state, v, pos, done, err = p.deliver(&stack, pos, v)
			if err == nil && done {
				return v, nil
			}
		}
		if err != nil {
			return Value{}, err
		}
	}
}

func (p *parser) expectValue(stack *[]jsonFrame, pos int) (jsonState, Value, int, error) {
	q := p.skipWS(pos)
	c, ok := p.at(q)
	if !ok {
		return 0, Value{}, 0, syntax(q, JsonSyntaxReasonExpectedValue)
	}
	if c != '{' && c != '[' {
		v, next, err := p.scalar(q)
		return deliverValue, v, next, err
	}
	// a container opened inside MaxDepth open ones is refused at its bracket, an empty one too
	// (canonical-encoding.md section 3): the first fault in reading order
	if len(*stack) >= MaxDepth {
		return 0, Value{}, 0, depthExceeded()
	}
	r := p.skipWS(q + 1)
	isObject := c == '{'
	closing := byte(']')
	if isObject {
		closing = '}'
	}
	if d, ok := p.at(r); ok && d == closing {
		var empty Value
		if isObject {
			empty, _ = mapOfSorted(nil, nil)
		} else {
			empty, _ = arrayOf(nil)
		}
		return deliverValue, empty, r + 1, nil
	}
	if isObject {
		*stack = append(*stack, jsonFrame{isObject: true, seen: &keySet{}})
		return expectKey, Value{}, r, nil
	}
	*stack = append(*stack, jsonFrame{})
	return expectValue, Value{}, r, nil
}

func (p *parser) expectKey(stack []jsonFrame, pos int) (jsonState, int, error) {
	q := p.skipWS(pos)
	if c, ok := p.at(q); !ok || c != '"' {
		return 0, 0, syntax(q, JsonSyntaxReasonExpectedMemberName)
	}
	name, next, err := p.str(q)
	if err != nil {
		return 0, 0, err
	}
	if len(stack) == 0 {
		return 0, 0, syntax(q, JsonSyntaxReasonExpectedValue)
	}
	f := &stack[len(stack)-1]
	// a member name is the caller's: compared, never hashed
	if f.seen.has(name) {
		return 0, 0, CanonicalErrorDuplicateKey{Key: previewOfWTF8(name)}
	}
	colon := p.skipWS(next)
	if c, ok := p.at(colon); !ok || c != ':' {
		return 0, 0, syntax(colon, JsonSyntaxReasonExpectedColon)
	}
	f.seen.add(name)
	f.key = name
	return expectValue, colon + 1, nil
}

// deliver hands a complete value to the enclosing array or object, closing it when its bracket follows.
func (p *parser) deliver(stack *[]jsonFrame, pos int, v Value) (jsonState, Value, int, bool, error) {
	q := p.skipWS(pos)
	c, have := p.at(q)
	if len(*stack) == 0 {
		if q == len(p.text) {
			return 0, v, 0, true, nil
		}
		return 0, Value{}, 0, false, syntax(q, JsonSyntaxReasonTrailingContent)
	}
	f := &(*stack)[len(*stack)-1]
	if !f.isObject {
		switch {
		case have && c == ',':
			f.items = append(f.items, v)
			return expectValue, Value{}, q + 1, false, nil
		case have && c == ']':
			items := append(f.items, v)
			*stack = (*stack)[:len(*stack)-1]
			a, err := arrayOf(items)
			return deliverValue, a, q + 1, false, err
		}
		return 0, Value{}, 0, false, syntax(q, JsonSyntaxReasonExpectedCommaOrBracket)
	}
	switch {
	case have && c == ',':
		f.names = append(f.names, f.key)
		f.vals = append(f.vals, v)
		return expectKey, Value{}, q + 1, false, nil
	case have && c == '}':
		names, vals := append(f.names, f.key), append(f.vals, v)
		*stack = (*stack)[:len(*stack)-1]
		m, err := object(names, vals)
		return deliverValue, m, q + 1, false, err
	}
	return 0, Value{}, 0, false, syntax(q, JsonSyntaxReasonExpectedCommaOrBrace)
}

// object is an object's members as a map, checked in member order as the reference does: an unpaired surrogate in a
// name, then the map's own checks.
func object(names []string, vals []Value) (Value, error) {
	entries := make([]Entry, len(names))
	for i, n := range names {
		if !utf8.ValidString(n) {
			return Value{}, CanonicalErrorUnpairedSurrogate{}
		}
		entries[i] = Entry{Key: n, Value: vals[i]}
	}
	return Map(entries...)
}

func (p *parser) scalar(q int) (Value, int, error) {
	rest := p.text[q:]
	c := p.text[q]
	switch {
	case c == '"':
		s, next, err := p.str(q)
		if err != nil {
			return Value{}, 0, err
		}
		if !utf8.ValidString(s) {
			return Value{}, 0, CanonicalErrorUnpairedSurrogate{}
		}
		return textOf(s), next, nil
	case c == 't' && strings.HasPrefix(rest, "true"):
		return Bool(true), q + 4, nil
	case c == 'f' && strings.HasPrefix(rest, "false"):
		return Bool(false), q + 5, nil
	case c == 'n' && strings.HasPrefix(rest, "null"):
		return Null(), q + 4, nil
	case c == '-' || (c >= '0' && c <= '9'):
		return p.number(q)
	}
	return Value{}, 0, syntax(q, JsonSyntaxReasonExpectedValue)
}

// number reads -? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?, converted exactly. Its digits are counted, then
// its scale checked, before any conversion. A refusal describes the number by its length and byte offset, never its
// text, whose size the caller chose.
func (p *parser) number(q int) (Value, int, error) {
	c, _ := p.at(q)
	negative := c == '-'
	intStart := q
	if negative {
		intStart++
	}
	intEnd := p.digits(intStart)
	dot, _ := p.at(intEnd)
	hasFrac := dot == '.' && intEnd < len(p.text)
	fracEnd := intEnd
	if hasFrac {
		fracEnd = p.digits(intEnd + 1)
	}
	e, _ := p.at(fracEnd)
	hasExp := (e == 'e' || e == 'E') && fracEnd < len(p.text)
	expSign, _ := p.at(fracEnd + 1)
	expStart := fracEnd + 1
	if hasExp && (expSign == '+' || expSign == '-') && fracEnd+1 < len(p.text) {
		expStart = fracEnd + 2
	}
	end := fracEnd
	if hasExp {
		end = p.digits(expStart)
	}
	invalid := func(bound Optional[NumberBound]) error {
		return CanonicalErrorInvalidNumber{Length: Some(uint64(end - q)), Offset: Some(uint64(q)), Bound: bound}
	}
	intDigits := p.text[intStart:intEnd]
	fracDigits := ""
	if hasFrac {
		fracDigits = p.text[intEnd+1 : fracEnd]
	}
	expDigits := "0"
	if hasExp {
		expDigits = p.text[expStart:end]
	}
	malformed := intDigits == "" || (len(intDigits) > 1 && intDigits[0] == '0') || (hasFrac && fracDigits == "") ||
		(hasExp && expDigits == "")
	if malformed {
		return Value{}, 0, invalid(None[NumberBound]())
	}
	// the integer digits have no leading zeros but a lone 0, after which the fraction's leading zeros are not counted
	significant := strings.TrimLeft(fracDigits, "0")
	significand := len(intDigits) + len(fracDigits)
	if intDigits == "0" {
		significand = len(significant)
	}
	exponentDigits := len(strings.TrimLeft(expDigits, "0"))
	for _, d := range []int{significand, exponentDigits} {
		if d > MaxDigits {
			return Value{}, 0, invalid(Some[NumberBound](NumberBoundDigits{Digits: uint64(d), Limit: MaxDigits}))
		}
	}
	// saturating: an exponent of more than 18 digits is far outside every accepted scale
	var magnitude int64
	for i := 0; i < len(expDigits); i++ {
		if magnitude > (1<<62)/10 {
			magnitude = 1 << 62
			break
		}
		magnitude = magnitude*10 + int64(expDigits[i]-'0')
	}
	exponent := magnitude
	if hasExp && expSign == '-' {
		exponent = -magnitude
	}
	scale := int64(len(fracDigits)) - exponent
	if scale > int64(p.maxScale) || scale < -int64(p.maxScale) {
		saturated := int32(min(max(scale, -1<<31), 1<<31-1))
		return Value{}, 0, invalid(Some[NumberBound](NumberBoundScale{Scale: saturated, Limit: uint64(p.maxScale)}))
	}
	// the digits as one integer: leading zeros (a lone 0 then the fraction's) add nothing, so at most MaxDigits are read
	all := strings.TrimLeft(intDigits+fracDigits, "0")
	unscaled := new(big.Int)
	if all != "" {
		if _, ok := unscaled.SetString(all, 10); !ok {
			return Value{}, 0, invalid(None[NumberBound]())
		}
	}
	if negative {
		unscaled.Neg(unscaled)
	}
	if scale >= 0 {
		v, err := decimal(unscaled, int(scale))
		return v, end, err
	}
	// a positive exponent can take an integer past MaxIntegerBits (9e9999): refused, as every producer does
	unscaled.Mul(unscaled, new(big.Int).Exp(big.NewInt(10), big.NewInt(-scale), nil))
	i, ok := newInt(unscaled)
	if !ok {
		return Value{}, 0, integerTooWide()
	}
	return IntValue(i), end, nil
}

// str reads a JSON string from its opening quote, as WTF-8, and gives the offset after the closing quote.
func (p *parser) str(open int) (string, int, error) {
	var out wtf8
	i := open + 1
	for {
		b, ok := p.at(i)
		if !ok {
			return "", 0, syntax(i, JsonSyntaxReasonUnterminatedString)
		}
		switch {
		case b == '"':
			return out.finish(), i + 1, nil
		case b < 0x20:
			return "", 0, syntax(i, JsonSyntaxReasonControlCharacter)
		case b == '\\':
			e, _ := p.at(i + 1)
			simple := byte(0)
			switch e {
			case '"', '\\', '/':
				simple = e
			case 'b':
				simple = 0x08
			case 'f':
				simple = 0x0c
			case 'n':
				simple = '\n'
			case 'r':
				simple = '\r'
			case 't':
				simple = '\t'
			case 'u':
			default:
				return "", 0, syntax(i, JsonSyntaxReasonInvalidEscape)
			}
			if e != 'u' {
				out.byte(simple)
				i += 2
				continue
			}
			if i+6 > len(p.text) {
				return "", 0, syntax(i, JsonSyntaxReasonInvalidEscape)
			}
			var unit uint16
			for _, h := range []byte(p.text[i+2 : i+6]) {
				d, ok := hexDigit(h)
				if !ok {
					return "", 0, syntax(i, JsonSyntaxReasonInvalidEscape)
				}
				unit = unit<<4 | uint16(d)
			}
			out.unit(unit)
			i += 6
		default:
			out.byte(b)
			i++
		}
	}
}

// wtf8 builds generalised UTF-8: a high surrogate escape followed directly by a low one is a pair; any other surrogate
// stays unpaired, in its 3-byte form.
type wtf8 struct {
	out     []byte
	high    uint16
	hasHigh bool
}

func (w *wtf8) flush() {
	if w.hasHigh {
		w.hasHigh = false
		w.lone(w.high)
	}
}

func (w *wtf8) lone(s uint16) {
	hi, lo := byte(s>>8), byte(s)
	w.out = append(w.out, 0xe0|hi>>4, 0x80|(hi&0x0f)<<2|lo>>6, 0x80|lo&0x3f)
}

func (w *wtf8) byte(b byte) {
	w.flush()
	w.out = append(w.out, b)
}

func (w *wtf8) unit(u uint16) {
	isHigh := u >= 0xd800 && u <= 0xdbff
	isLow := u >= 0xdc00 && u <= 0xdfff
	if w.hasHigh {
		w.hasHigh = false
		if isLow {
			// the 20-bit payload is added to 0x10000, not ORed: payloads from U+20000 up have bit 16 set
			w.out = utf8.AppendRune(w.out, rune((uint32(w.high&0x3ff)<<10|uint32(u&0x3ff))+0x10000))
			return
		}
		w.lone(w.high)
	}
	switch {
	case isHigh:
		w.high, w.hasHigh = u, true
	case isLow:
		w.lone(u)
	default:
		w.out = utf8.AppendRune(w.out, rune(u))
	}
}

func (w *wtf8) finish() string {
	w.flush()
	return string(w.out)
}

// ------------------------------------------------------------------------------------------------ render

// RenderJSONWithin is the compact JSON of v, object members in code-point order of their names, if it is at most maxBytes
// long in UTF-8: a negative limit is 0, one past MaxRenderBytes is MaxRenderBytes. It refuses at the first fault it
// meets in writing order: a part that would pass the limit is TooLarge with the limit as applied, and what JSON cannot
// write is CanonicalErrorNotJsonRepresentable (bytes, a rational whose decimal does not terminate, a number past the
// bounds ParseJSON accepts). Never recursive: its explicit stack holds at most MaxDepth open containers.
//
// O(min(weight, limit)) apart from sorting object members: a value's JSON is at least as long in UTF-8 as its weight,
// so each part is refused in O(1) when its weight is over what is left; escaped text is counted before it is written.
func RenderJSONWithin(v Value, maxBytes int) (string, error) {
	var b strings.Builder
	out := renderBudget{out: &b, max: byteLimit(maxBytes, MaxRenderBytes)}
	if err := writeJSON(&out, v); err != nil {
		return "", err
	}
	return b.String(), nil
}

// textOut is where rendered text goes: each byte of the rendering once, in order, and nothing else.
type textOut interface {
	WriteString(s string) (int, error)
}

type renderBudget struct {
	out  textOut
	used int
	max  int
}

func (b *renderBudget) refused() error { return TooLarge{Limit: b.max} }

func (b *renderBudget) left() int { return b.max - b.used }

// admits reports whether a part of at least weight bytes can still fit.
func (b *renderBudget) admits(weight uint64) error {
	if weight > uint64(b.left()) {
		return b.refused()
	}
	return nil
}

func (b *renderBudget) push(piece string) error {
	if len(piece) > b.left() {
		return b.refused()
	}
	b.out.WriteString(piece)
	b.used += len(piece)
	return nil
}

// quote writes s quoted and escaped, counted before it is written: runs that need no escape go straight from s.
func (b *renderBudget) quote(s string) error {
	length, fits := quotedLenWithin(s, b.left())
	if !fits {
		return b.refused()
	}
	b.out.WriteString(`"`)
	run := 0
	for i := 0; i < len(s); i++ {
		if e := escape(s[i]); e != "" {
			if run < i {
				b.out.WriteString(s[run:i])
			}
			b.out.WriteString(e)
			run = i + 1
		}
	}
	if run < len(s) {
		b.out.WriteString(s[run:])
	}
	b.out.WriteString(`"`)
	b.used += length
	return nil
}

// escape is how JSON writes a byte it escapes (the quote, the backslash and the controls below U+0020, all ASCII), or
// "" for a byte written as is.
func escape(c byte) string {
	const hex = "0123456789abcdef"
	switch c {
	case '"':
		return `\"`
	case '\\':
		return `\\`
	case '\n':
		return `\n`
	case '\r':
		return `\r`
	case '\t':
		return `\t`
	case 0x08:
		return `\b`
	case 0x0c:
		return `\f`
	}
	if c < 0x20 {
		return `\u00` + hex[c>>4:c>>4+1] + hex[c&0xf:c&0xf+1]
	}
	return ""
}

// quotedLenWithin is the length of s quoted and escaped as quote writes it, in UTF-8 bytes, if it is at most limit.
func quotedLenWithin(s string, limit int) (int, bool) {
	n := 2
	for i := 0; i < len(s) && n <= limit; i++ {
		if e := escape(s[i]); e != "" {
			n += len(e)
		} else {
			n++
		}
	}
	// counted no further than one escape past the limit, so the count never overflows an int (a 32-bit one included)
	return n, n <= limit
}

// renderOpen is an array or object being rendered: the elements still to come, in writing order.
type renderOpen struct {
	n       *node
	order   []int // an object's members, in code-point order of their names
	next    int
	started bool
}

func writeJSON(out *renderBudget, v Value) error {
	var stack []renderOpen
	next, have := v, true
	for {
		if have {
			if err := out.admits(next.Weight()); err != nil {
				return err
			}
			if err := writeJSONNode(out, next, &stack); err != nil {
				return err
			}
		}
		have = false
		if len(stack) == 0 {
			return nil
		}
		top := &stack[len(stack)-1]
		count := len(top.n.items)
		if top.n.kind == KindMap {
			count = len(top.n.keys)
		}
		if top.next >= count {
			closing := "]"
			if top.n.kind == KindMap {
				closing = "}"
			}
			if err := out.push(closing); err != nil {
				return err
			}
			stack = stack[:len(stack)-1]
			continue
		}
		if top.started {
			if err := out.push(","); err != nil {
				return err
			}
		}
		top.started = true
		if top.n.kind == KindArray {
			next = top.n.items[top.next]
		} else {
			i := top.order[top.next]
			if err := out.quote(top.n.keys[i]); err != nil {
				return err
			}
			if err := out.push(":"); err != nil {
				return err
			}
			next = top.n.vals[i]
		}
		top.next++
		have = true
	}
}

func writeJSONNode(out *renderBudget, v Value, stack *[]renderOpen) error {
	n := v.n
	switch v.Kind() {
	case KindNull:
		return out.push("null")
	case KindBool:
		if n.b {
			return out.push("true")
		}
		return out.push("false")
	case KindInt:
		// every integer is within MaxIntegerBits, so it has at most MaxDigits digits: converting it is bounded work
		return out.push(n.num.String())
	case KindRational:
		d, what, ok := decimalText(Rational{num: n.num, den: n.den})
		if !ok {
			return CanonicalErrorNotJsonRepresentable{What: what}
		}
		return out.push(d)
	case KindText:
		return out.quote(n.text)
	case KindBytes:
		return CanonicalErrorNotJsonRepresentable{What: NotJsonBytes}
	case KindArray:
		if err := out.push("["); err != nil {
			return err
		}
		*stack = append(*stack, renderOpen{n: n})
		return nil
	case KindMap:
		if err := out.push("{"); err != nil {
			return err
		}
		order := make([]int, len(n.keys))
		for i := range order {
			order[i] = i
		}
		// Go orders strings by bytes, which for UTF-8 is code-point order
		slices.SortFunc(order, func(a, b int) int { return strings.Compare(n.keys[a], n.keys[b]) })
		*stack = append(*stack, renderOpen{n: n, order: order})
		return nil
	}
	return nil
}

// within reports whether a plain decimal's significant digits and scale are within the bounds ParseJSON accepts.
func within(text string) bool {
	unsigned := strings.TrimPrefix(text, "-")
	intPart, fracPart, _ := strings.Cut(unsigned, ".")
	significand := len(strings.TrimLeft(intPart, "0")) + len(fracPart)
	if intPart == "0" {
		significand = len(strings.TrimLeft(fracPart, "0"))
	}
	return significand <= MaxDigits && len(fracPart) <= DefaultMaxScale
}

// microBitsPerFive is log2(5) in millionths, rounded down: the bits per factor of five.
const microBitsPerFive = 2_321_928

// twosAndFives is (a, b) when d = 2^a 5^b, so that 1/d terminates, and the big-number divisions and exponentiations
// it took: at most four, whatever d. The twos are the low zero bits; the power of five is estimated from the bit
// length (5^b has floor(b log2 5) + 1 bits, and the neighbours absorb any rounding) and checked by exponentiation,
// never found one division per factor (14,306 of them for 5^14306, the widest a rational's denominator is).
func twosAndFives(d *big.Int) (twos, fives uint64, ok bool, steps int) {
	twos = uint64(d.TrailingZeroBits())
	rest := new(big.Int).Rsh(d, uint(twos))
	if rest.Cmp(big.NewInt(1)) == 0 {
		return twos, 0, true, 0
	}
	steps = 1
	if new(big.Int).Rem(rest, big.NewInt(5)).Sign() != 0 {
		return 0, 0, false, steps
	}
	estimate := (uint64(rest.BitLen()) - 1) * 1_000_000 / microBitsPerFive
	// b log2 5 lies in [bits - 1, bits), under half a factor wide, so b is the estimate's ceiling but for rounding
	for _, b := range []uint64{estimate + 1, estimate, estimate - 1} {
		if b == 0 || b > estimate+1 {
			continue
		}
		steps++
		if new(big.Int).Exp(big.NewInt(5), new(big.Int).SetUint64(b), nil).Cmp(rest) == 0 {
			return twos, b, true, steps
		}
	}
	return 0, 0, false, steps
}

// decimalText is r's exact decimal, within the bounds ParseJSON accepts; otherwise why there is none:
// NotJsonNonTerminatingRational when its denominator has prime factors other than 2 and 5, NotJsonOverLongNumber
// past the bounds, both found from the denominator's factors (twosAndFives) before any digit is produced.
func decimalText(r Rational) (string, NotJson, bool) {
	d := r.den.big()
	twos, fives, ok, _ := twosAndFives(d)
	if !ok {
		return "", NotJsonNonTerminatingRational, false
	}
	scale := max(twos, fives)
	if scale > DefaultMaxScale {
		return "", NotJsonOverLongNumber, false
	}
	// 10^scale is a multiple of d, so the division is exact
	factor := new(big.Int).Exp(big.NewInt(10), new(big.Int).SetUint64(scale), nil)
	factor.Quo(factor, d)
	scaled := factor.Mul(factor, r.num.big())
	digits := new(big.Int).Abs(scaled).String()
	if pad := int(scale) + 1 - len(digits); pad > 0 {
		digits = strings.Repeat("0", pad) + digits
	}
	cut := len(digits) - int(scale)
	sign := ""
	if scaled.Sign() < 0 {
		sign = "-"
	}
	text := sign + digits[:cut] + "." + digits[cut:]
	if !within(text) {
		return "", NotJsonOverLongNumber, false
	}
	return text, "", true
}

// Decimal is r's exact decimal, as RenderJSON writes it: CanonicalErrorNotJsonRepresentable when it does not
// terminate, or is past the bounds ParseJSON accepts.
func Decimal(r Rational) (string, error) {
	if r.den.Sign() == 0 {
		return "", typeMismatch("expected a rational")
	}
	text, what, ok := decimalText(r)
	if !ok {
		return "", CanonicalErrorNotJsonRepresentable{What: what}
	}
	return text, nil
}

// decimal is unscaled / 10^scale, reduced with only the primes 2 and 5 (a JSON decimal's denominator has no others):
// an integer, refused as CanonicalErrorIntegerTooWide past MaxIntegerBits, or a rational, refused as
// CanonicalErrorRationalTooWide past MaxRationalBits (0.000...1 at a scale of 10,000 has the denominator 10^10000).
// unscaled is this package's, and may be changed.
//
// 10^scale's only primes are 2 and 5, so the common factor is 2^min(v2, scale) 5^min(v5, scale): the twos go in one
// shift, and the fives by trial division by 5^(2^i) from the largest 2^i <= scale down, which builds min(v5, scale)
// bit by bit (about 14 divisions for a scale of 10000). The same result as Number against 10^scale, without its
// general gcd.
func decimal(unscaled *big.Int, scale int) (Value, error) {
	if unscaled.Sign() == 0 {
		return IntValue(Int{}), nil
	}
	negative := unscaled.Sign() < 0
	magnitude := new(big.Int).Abs(unscaled)
	twos := min(int(magnitude.TrailingZeroBits()), scale)
	magnitude.Rsh(magnitude, uint(twos))
	rest, fives := stripFives(magnitude, scale)
	if negative {
		rest.Neg(rest)
	}
	denominator := new(big.Int).Exp(big.NewInt(5), big.NewInt(int64(scale-fives)), nil)
	denominator.Lsh(denominator, uint(scale-twos))
	if denominator.Cmp(big.NewInt(1)) == 0 {
		if i, ok := newInt(rest); ok {
			return IntValue(i), nil
		}
		return Value{}, integerTooWide()
	}
	if v, ok := rationalValue(rest, denominator); ok {
		return v, nil
	}
	return Value{}, rationalTooWide()
}

// stripFives is n without its factors of 5, at most limit of them, and how many were removed.
func stripFives(n *big.Int, limit int) (*big.Int, int) {
	type power struct {
		e int
		p *big.Int
	}
	var powers []power
	for e, p := 1, big.NewInt(5); e <= limit; e, p = e*2, new(big.Int).Mul(p, p) {
		powers = append(powers, power{e, p})
		if e > limit/2 {
			break
		}
	}
	count := 0
	q, r := new(big.Int), new(big.Int)
	for i := len(powers) - 1; i >= 0; i-- {
		pw := powers[i]
		if count+pw.e > limit {
			continue
		}
		q.QuoRem(n, pw.p, r)
		if r.Sign() == 0 {
			n, q = q, n
			count += pw.e
		}
	}
	return n, count
}
