package core

import (
	"bytes"
	"hash"
	"math/big"
	"math/bits"
	"slices"
	"unicode/utf8"
)

const (
	tagPositiveBignum = 2
	tagNegativeBignum = 3
	tagRational       = 30

	majorUnsigned = 0
	majorNegative = 1
	majorBytes    = 2
	majorText     = 3
	majorArray    = 4
	majorMap      = 5
	majorTag      = 6
	majorSimple   = 7
)

// ------------------------------------------------------------------------------------------------ encoding

// Encode is the canonical encoding of v: EncodeWithin at MaxEncodedBytes. A value whose encoding is longer (one built
// with shared parts can weigh up to 2^63 - 1) is TooLarge, refused from its weight in O(1) when that is over.
func Encode(v Value) ([]byte, error) { return EncodeWithin(v, MaxEncodedBytes) }

// EncodeWithin is the canonical encoding of v, if it is at most maxBytes long, and otherwise TooLarge with the limit as
// applied: a negative limit is 0, one past MaxEncodedBytes is MaxEncodedBytes. O(min(weight, limit)), never holding
// more than the limit: no encoding is shorter than its value's weight, so a weight over the limit is refused at once,
// and each node writes a byte before its children, so the walk stops within limit + 1 nodes.
func EncodeWithin(v Value, maxBytes int) ([]byte, error) {
	limit := byteLimit(maxBytes, MaxEncodedBytes)
	if v.Weight() > uint64(limit) {
		return nil, TooLarge{Limit: limit}
	}
	out := &budget{max: limit, out: make([]byte, 0, min(limit, 64)), writing: true}
	if !writeValue(out, v) {
		return nil, TooLarge{Limit: limit}
	}
	return out.out, nil
}

// EncodedLengthWithin is the length of v's canonical encoding, if it is at most maxBytes, counted without encoding it,
// with the bounds and the refusal of EncodeWithin.
func EncodedLengthWithin(v Value, maxBytes int) (int, error) {
	limit := byteLimit(maxBytes, MaxEncodedBytes)
	if v.Weight() > uint64(limit) {
		return 0, TooLarge{Limit: limit}
	}
	out := &budget{max: limit}
	if !writeValue(out, v) {
		return 0, TooLarge{Limit: limit}
	}
	return out.len, nil
}

// byteLimit is maxBytes as applied: a negative limit is 0, and none is above ceiling.
func byteLimit(maxBytes, ceiling int) int { return max(0, min(maxBytes, ceiling)) }

// sink is where encoded bytes go: put and putString give the next bytes, room asks whether n more fit before they are
// produced. Each reports false once the output would pass its budget.
type sink interface {
	put(b []byte) bool
	putString(s string) bool
	// putHead gives the first n bytes of b: a head, passed by value so that writing one allocates nothing
	putHead(b [9]byte, n int) bool
	room(n uint64) bool
}

// budget is a byte budget, writing its output when writing is set, else only counting it: its buffer never grows
// past max.
type budget struct {
	out     []byte
	len     int
	max     int
	writing bool
}

func (b *budget) reach(n uint64) (int, bool) {
	if n > uint64(b.max-b.len) {
		return 0, false
	}
	return b.len + int(n), true
}

func (b *budget) grow(next int) {
	if cap(b.out) < next {
		grown := make([]byte, len(b.out), min(max(next, 2*cap(b.out)), b.max))
		copy(grown, b.out)
		b.out = grown
	}
}

func (b *budget) put(p []byte) bool {
	next, ok := b.reach(uint64(len(p)))
	if !ok {
		return false
	}
	if b.writing {
		b.grow(next)
		b.out = append(b.out, p...)
	}
	b.len = next
	return true
}

func (b *budget) putString(s string) bool {
	next, ok := b.reach(uint64(len(s)))
	if !ok {
		return false
	}
	if b.writing {
		b.grow(next)
		b.out = append(b.out, s...)
	}
	b.len = next
	return true
}

func (b *budget) putHead(h [9]byte, n int) bool {
	next, ok := b.reach(uint64(n))
	if !ok {
		return false
	}
	if b.writing {
		b.grow(next)
		b.out = append(b.out, h[:n]...)
	}
	b.len = next
	return true
}

func (b *budget) room(n uint64) bool {
	_, ok := b.reach(n)
	return ok
}

// StagedBytes is the most bytes the hashing sink stages: the walk's small pieces (heads, keys, limbs) reach the hash
// this many at a time instead of one call each.
const StagedBytes = 1024

// staged writes into a hash: small pieces are staged in a fixed array and absorbed together; a piece that does not fit
// goes to the hash in place. It holds at most StagedBytes of the encoding; flush when done.
type staged struct {
	into hash.Hash
	buf  [StagedBytes]byte
	n    int
}

func (s *staged) flush() {
	if s.n > 0 {
		s.into.Write(s.buf[:s.n])
	}
	s.n = 0
}

func (s *staged) put(p []byte) bool {
	if len(p) <= StagedBytes-s.n {
		s.n += copy(s.buf[s.n:], p)
		return true
	}
	s.flush()
	if len(p) <= StagedBytes {
		s.n = copy(s.buf[:], p)
		return true
	}
	s.into.Write(p)
	return true
}

// putString goes through the staging array in pieces, so a text is never copied whole into a []byte.
func (s *staged) putString(t string) bool {
	for len(t) > 0 {
		if s.n == StagedBytes {
			s.flush()
		}
		c := copy(s.buf[s.n:], t)
		s.n += c
		t = t[c:]
	}
	return true
}

func (s *staged) putHead(h [9]byte, n int) bool {
	if n > StagedBytes-s.n {
		s.flush()
	}
	s.n += copy(s.buf[s.n:], h[:n])
	return true
}

func (s *staged) room(uint64) bool { return true }

// open is an array or map being written: the elements still to come.
type open struct {
	n    *node
	next int
}

// writeValue writes v's canonical encoding to out; false when out's budget refused. Never recursive: an explicit stack
// holds one open container per level of nesting (at most MaxDepth). Every node writes its first byte before its
// children (the budget relies on it).
func writeValue(out sink, v Value) bool {
	var stack []open
	next, have := v, true
	for {
		if have {
			if !writeNode(out, next, &stack) {
				return false
			}
		}
		have = false
		if len(stack) == 0 {
			return true
		}
		top := &stack[len(stack)-1]
		switch top.n.kind {
		case KindArray:
			if top.next < len(top.n.items) {
				next, have = top.n.items[top.next], true
				top.next++
			}
		case KindMap:
			if top.next < len(top.n.keys) {
				if !writeText(out, top.n.keys[top.next]) {
					return false
				}
				next, have = top.n.vals[top.next], true
				top.next++
			}
		}
		if !have {
			stack = stack[:len(stack)-1]
		}
	}
}

// writeNode writes a scalar whole, or a container's head, opening it on the stack.
func writeNode(out sink, v Value, stack *[]open) bool {
	n := v.n
	switch v.Kind() {
	case KindNull:
		return out.putHead([9]byte{0xf6}, 1)
	case KindBool:
		if n.b {
			return out.putHead([9]byte{0xf5}, 1)
		}
		return out.putHead([9]byte{0xf4}, 1)
	case KindInt:
		return writeInteger(out, n.num)
	case KindRational:
		return head(out, majorTag, tagRational) && head(out, majorArray, 2) && writeInteger(out, n.num) &&
			writeInteger(out, n.den)
	case KindText:
		return writeText(out, n.text)
	case KindBytes:
		return head(out, majorBytes, uint64(len(n.bytes))) && out.put(n.bytes)
	case KindArray:
		if !head(out, majorArray, uint64(len(n.items))) {
			return false
		}
		*stack = append(*stack, open{n: n})
		return true
	case KindMap:
		if !head(out, majorMap, uint64(len(n.keys))) {
			return false
		}
		*stack = append(*stack, open{n: n})
		return true
	}
	return true
}

func writeText(out sink, s string) bool {
	return head(out, majorText, uint64(len(s))) && out.putString(s)
}

// writeInteger writes i: major type 0 for i >= 0 and 1 (carrying -1 - i) for i < 0 within 64 bits, else tag 2 or 3
// around the big-endian magnitude of i or -1 - i. Its length is known from its bit length, so the budget is asked
// before any of the magnitude is written, and the magnitude is streamed a word at a time from i's own words, so
// nothing the size of i is allocated.
func writeInteger(out sink, i Int) bool {
	n := i.big()
	negative := n.Sign() < 0
	major := uint64(majorUnsigned)
	if negative {
		major = majorNegative
	}
	if i.bits <= 64 {
		if !negative {
			return head(out, byte(major), n.Uint64())
		}
		if n.IsInt64() {
			return head(out, byte(major), uint64(^n.Int64()))
		}
		return head(out, byte(major), new(big.Int).Not(n).Uint64())
	}
	size := (i.bits + 7) / 8
	tag := uint64(tagPositiveBignum)
	if negative {
		tag = tagNegativeBignum
	}
	if !head(out, majorTag, tag) || !head(out, majorBytes, size) || !out.room(size) {
		return false
	}
	return writeMagnitude(out, n, negative, size)
}

// writeMagnitude streams the size big-endian bytes of |n| (n >= 0), or of |n| - 1 (n < 0, that is -1 - n), from n's
// words, most significant first. |n| - 1 borrows through |n|'s zero words below its lowest non-zero one: those words
// are all ones, that one loses 1, and the words above it are unchanged. n's words are only read.
func writeMagnitude(out sink, n *big.Int, negative bool, size uint64) bool {
	words := n.Bits()
	const wordBytes = bits.UintSize / 8
	lowest := 0
	if negative {
		for lowest < len(words) && words[lowest] == 0 {
			lowest++
		}
	}
	word := func(k int) uint64 {
		w := uint64(words[k])
		if !negative || k > lowest {
			return w
		}
		if k < lowest {
			return 1<<bits.UintSize - 1
		}
		return w - 1
	}
	// the top word carries only the significant bytes; any word above them (|n| = 2^(w k), borrowed to zero) none
	count := int((size + wordBytes - 1) / wordBytes)
	top := int(size) - (count-1)*wordBytes
	for k := count - 1; k >= 0; k-- {
		w := word(k)
		var b [9]byte
		from := 0
		if k == count-1 {
			from = wordBytes - top
		}
		for j := 0; j < wordBytes; j++ {
			b[j] = byte(w >> (8 * (wordBytes - 1 - j)))
		}
		var piece [9]byte
		copy(piece[:], b[from:wordBytes])
		if !out.putHead(piece, wordBytes-from) {
			return false
		}
	}
	return true
}

// head writes the shortest-form initial byte and argument.
func head(out sink, major byte, argument uint64) bool {
	mt := major << 5
	switch {
	case argument < 24:
		return out.putHead([9]byte{mt | byte(argument)}, 1)
	case argument < 1<<8:
		return out.putHead([9]byte{mt | 0x18, byte(argument)}, 2)
	case argument < 1<<16:
		return out.putHead([9]byte{mt | 0x19, byte(argument >> 8), byte(argument)}, 3)
	case argument < 1<<32:
		return out.putHead([9]byte{mt | 0x1a, byte(argument >> 24), byte(argument >> 16), byte(argument >> 8), byte(argument)}, 5)
	}
	return out.putHead([9]byte{mt | 0x1b, byte(argument >> 56), byte(argument >> 48), byte(argument >> 40),
		byte(argument >> 32), byte(argument >> 24), byte(argument >> 16), byte(argument >> 8), byte(argument)}, 9)
}

// MagnitudeBytes is the big-endian magnitude of n without leading zero bytes (empty for 0): the content of a
// bignum's byte string for n or for -1 - n.
func MagnitudeBytes(n *big.Int) []byte {
	if n == nil {
		return []byte{}
	}
	return new(big.Int).Abs(n).Bytes()
}

// ------------------------------------------------------------------------------------------------ decoding

// Decode strictly decodes one canonical value (docs/specs/canonical-encoding.md section 2): any other encoding of a
// value, trailing bytes, or nesting deeper than MaxDepth is refused with the CanonicalError kind of the first fault in
// reading order. When it succeeds, Encode(Decode(b)) is b. The input is copied once, first, and the copy decoded: the
// decoder reads some bytes twice (a key for its order, a bignum for its width then its value), and a caller's slice
// may change meanwhile (another goroutine's write), so both reads are of bytes no caller can reach. Nothing decoded
// holds a view of either.
func Decode(b []byte) (Value, error) { return DecodeWithLimit(b, MaxDepth) }

// DecodeWithLimit is Decode with a lower nesting limit (a negative one is 0, and none is above MaxDepth). The decoder
// keeps its own frame stack, so input of any nesting is a typed error, never a stack overflow.
func DecodeWithLimit(b []byte, maxDepth int) (Value, error) {
	return decodeWith(slices.Clone(b), max(0, min(maxDepth, MaxDepth)), exactRational, magnitudeOf)
}

// reducer builds a decoded rational from parts within MaxRationalBits: exactRational, which runs the one gcd that
// checks lowest terms. A test passes one that counts its calls and the widths it sees.
type reducer func(n, d *big.Int) (Value, bool)

// magnituder builds a bignum's magnitude from its bytes, once their form and width are checked: magnitudeOf. A test
// passes one that counts its calls.
type magnituder func(b []byte) *big.Int

func magnitudeOf(b []byte) *big.Int { return new(big.Int).SetBytes(b) }

func decodeWith(input []byte, maxDepth int, reduce reducer, magnitude magnituder) (Value, error) {
	d := decoder{input: input, maxDepth: maxDepth, reduce: reduce, magnitude: magnitude}
	return d.run()
}

// magnitudeBits is the Java bitLength of a bignum's integer from its magnitude's bytes, b[0] non-zero: 8 bits per byte
// after the first, and the first byte's own. m and -1 - m have the same bitLength, so it holds for either tag.
func magnitudeBits(b []byte) uint64 {
	if len(b) == 0 {
		return 0
	}
	return uint64(len(b)-1)*8 + uint64(bits.Len8(b[0]))
}

type itemKind uint8

const (
	itemScalar itemKind = iota
	itemArray
	itemMap
)

type item struct {
	kind  itemKind
	value Value
	count uint64
}

type frame struct {
	isMap     bool
	remaining uint64
	items     []Value
	keys      []string
	vals      []Value
	// the span of the last key's encoding, for the order check
	lastStart, lastEnd int
	hasLast            bool
	// a key read, waiting for its value
	pending                  string
	pendingStart, pendingEnd int
	hasPending               bool
}

type decoder struct {
	input     []byte
	maxDepth  int
	stack     []frame
	reduce    reducer
	magnitude magnituder
}

func (d *decoder) run() (Value, error) {
	pos := 0
	for {
		it, end, err := d.readItem(pos)
		if err != nil {
			return Value{}, err
		}
		var v Value
		if it.kind != itemScalar && len(d.stack) >= d.maxDepth {
			return Value{}, CanonicalErrorDepthExceeded{Limit: uint64(d.maxDepth)}
		}
		switch {
		case it.kind == itemScalar:
			v = it.value
		case it.count == 0 && it.kind == itemArray:
			v, _ = arrayOf(nil)
		case it.count == 0:
			v, _ = mapOfSorted(nil, nil)
		case it.kind == itemArray:
			d.stack = append(d.stack, frame{remaining: it.count})
			pos = end
			continue
		default:
			d.stack = append(d.stack, frame{isMap: true, remaining: it.count})
			pos = end
			continue
		}
		next, done, err := d.deliver(v, pos, end)
		if err != nil {
			return Value{}, err
		}
		if done {
			return next, nil
		}
		pos = end
	}
}

// deliver hands a complete value (encoded at start..end) to the enclosing frame, closing every frame it completes; it
// gives the decoded value when the top level is complete.
func (d *decoder) deliver(v Value, start, end int) (Value, bool, error) {
	for {
		if len(d.stack) == 0 {
			if end == len(d.input) {
				return v, true, nil
			}
			return Value{}, false, CanonicalErrorTrailingBytes{Offset: uint64(end)}
		}
		f := &d.stack[len(d.stack)-1]
		switch {
		case !f.isMap:
			f.items = append(f.items, v)
			f.remaining--
		case !f.hasPending:
			key, ok := v.AsText()
			if !ok {
				return Value{}, false, CanonicalErrorUnsupported{Offset: uint64(start), Item: UnsupportedItemNonTextKey{}}
			}
			if f.hasLast && bytes.Compare(d.input[f.lastStart:f.lastEnd], d.input[start:end]) >= 0 {
				return Value{}, false, CanonicalErrorNonCanonical{Offset: uint64(start), Reason: NonCanonicalReasonKeyOrder}
			}
			f.pending, f.pendingStart, f.pendingEnd, f.hasPending = key, start, end, true
			return Value{}, false, nil
		default:
			if v.IsNull() {
				return Value{}, false, CanonicalErrorNonCanonical{Offset: uint64(start), Reason: NonCanonicalReasonNullMapValue}
			}
			f.lastStart, f.lastEnd, f.hasLast, f.hasPending = f.pendingStart, f.pendingEnd, true, false
			f.keys = append(f.keys, f.pending)
			f.vals = append(f.vals, v)
			f.remaining--
		}
		if f.remaining > 0 {
			return Value{}, false, nil
		}
		closed := *f
		d.stack = d.stack[:len(d.stack)-1]
		var err error
		if closed.isMap {
			v, err = mapOfSorted(closed.keys, closed.vals)
		} else {
			v, err = arrayOf(closed.items)
		}
		if err != nil {
			return Value{}, false, err
		}
		// the start of a closed container is not tracked; only a key's start is ever used, and a container delivered
		// as a key is refused before its offset matters
		start = end
	}
}

type cborHead struct {
	major    byte
	argument uint64
	end      int
}

func (d *decoder) readItem(pos int) (item, int, error) {
	h, err := d.readHead(pos)
	if err != nil {
		return item{}, 0, err
	}
	switch h.major {
	case majorUnsigned:
		return item{value: Uint64(h.argument)}, h.end, nil
	case majorNegative:
		// -1 - argument, which fits an int64 exactly when argument does
		if h.argument <= 1<<63-1 {
			return item{value: Int64(-1 - int64(h.argument))}, h.end, nil
		}
		n := new(big.Int).SetUint64(h.argument)
		n.Not(n)
		return item{value: IntValue(Int{v: n, bits: bitLength(n)})}, h.end, nil
	case majorBytes:
		b, e, err := d.span(h.end, h.argument)
		if err != nil {
			return item{}, 0, err
		}
		return item{value: Bytes(b)}, e, nil
	case majorText:
		b, e, err := d.span(h.end, h.argument)
		if err != nil {
			return item{}, 0, err
		}
		if !utf8.Valid(b) {
			return item{}, 0, CanonicalErrorInvalidUtf8{Offset: uint64(h.end)}
		}
		return item{value: textOf(string(b))}, e, nil
	case majorArray:
		n, err := d.count(h.end, h.argument, 1)
		return item{kind: itemArray, count: n}, h.end, err
	case majorMap:
		n, err := d.count(h.end, h.argument, 2)
		return item{kind: itemMap, count: n}, h.end, err
	case majorTag:
		v, e, err := d.readTagged(pos, h.argument, h.end)
		return item{value: v}, e, err
	}
	switch h.argument {
	case 20:
		return item{value: Bool(false)}, h.end, nil
	case 21:
		return item{value: Bool(true)}, h.end, nil
	case 22:
		return item{value: Null()}, h.end, nil
	}
	return item{}, 0, CanonicalErrorUnsupported{Offset: uint64(pos), Item: UnsupportedItemSimpleValue{Value: uint32(h.argument)}}
}

func (d *decoder) readHead(pos int) (cborHead, error) {
	if pos >= len(d.input) {
		return cborHead{}, CanonicalErrorUnexpectedEnd{Offset: uint64(pos)}
	}
	initial := d.input[pos]
	major, info := initial>>5, initial&0x1f
	next := pos + 1
	sized := func(n uint64, minimum uint64) (cborHead, error) {
		b, end, err := d.span(next, n)
		if err != nil {
			return cborHead{}, err
		}
		var argument uint64
		for _, x := range b {
			argument = argument<<8 | uint64(x)
		}
		if argument < minimum {
			return cborHead{}, CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: NonCanonicalReasonNonShortestArgument}
		}
		return cborHead{major: major, argument: argument, end: end}, nil
	}
	switch {
	case info < 24:
		return cborHead{major: major, argument: uint64(info), end: next}, nil
	case major == majorSimple:
		return cborHead{}, CanonicalErrorUnsupported{Offset: uint64(pos), Item: UnsupportedItemFloatOrSimple{Info: uint32(info)}}
	}
	switch info {
	case 24:
		return sized(1, 24)
	case 25:
		return sized(2, 1<<8)
	case 26:
		return sized(4, 1<<16)
	case 27:
		return sized(8, 1<<32)
	case 31:
		return cborHead{}, CanonicalErrorUnsupported{Offset: uint64(pos), Item: UnsupportedItemIndefiniteLength{}}
	}
	return cborHead{}, CanonicalErrorUnsupported{Offset: uint64(pos), Item: UnsupportedItemReservedInfo{Info: uint32(info)}}
}

func (d *decoder) readTagged(pos int, tag uint64, end int) (Value, int, error) {
	switch tag {
	case tagPositiveBignum, tagNegativeBignum:
		n, e, err := d.readBignum(pos, tag, end, MaxIntegerBits, integerTooWide)
		if err != nil {
			return Value{}, 0, err
		}
		i, ok := newInt(n)
		if !ok {
			return Value{}, 0, integerTooWide()
		}
		return IntValue(i), e, nil
	case tagRational:
		return d.readRational(pos, end)
	}
	return Value{}, 0, CanonicalErrorUnsupported{Offset: uint64(pos), Item: UnsupportedItemTag{Tag: tag}}
}

// readBignum reads a bignum's integer, in order: a byte string (NonCanonical), present in full (UnexpectedEnd), in
// canonical form (NonCanonical), and at most limit wide (tooWide), all before the integer is built.
func (d *decoder) readBignum(pos int, tag uint64, end int, limit uint64, tooWide func() error) (*big.Int, int, error) {
	body, err := d.readHead(end)
	if err != nil {
		return nil, 0, err
	}
	if body.major != majorBytes {
		return nil, 0, CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: NonCanonicalReasonBignumNotBytes}
	}
	b, e, err := d.span(body.end, body.argument)
	if err != nil {
		return nil, 0, err
	}
	switch {
	case len(b) == 0 || b[0] == 0:
		return nil, 0, CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: NonCanonicalReasonBignumLeadingZero}
	case len(b) <= 8:
		return nil, 0, CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: NonCanonicalReasonBignumInRange}
	case magnitudeBits(b) > limit:
		return nil, 0, tooWide()
	}
	m := d.magnitude(b)
	if tag == tagNegativeBignum {
		m.Not(m)
	}
	return m, e, nil
}

// readRational reads, in order: the pair, the numerator and its width, the denominator and its width, the
// denominator's range, and last the one gcd that checks lowest terms (docs/specs/conformance.md section 5).
func (d *decoder) readRational(pos, end int) (Value, int, error) {
	nonCanonical := func(r NonCanonicalReason) error {
		return CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: r}
	}
	pair, err := d.readHead(end)
	if err != nil {
		return Value{}, 0, err
	}
	if pair.major != majorArray || pair.argument != 2 {
		return Value{}, 0, nonCanonical(NonCanonicalReasonRationalNotPair)
	}
	n, afterNumerator, err := d.readPart(pair.end)
	if err != nil {
		return Value{}, 0, err
	}
	den, after, err := d.readPart(afterNumerator)
	if err != nil {
		return Value{}, 0, err
	}
	if den.Cmp(big.NewInt(2)) < 0 {
		return Value{}, 0, nonCanonical(NonCanonicalReasonRationalDenominator)
	}
	v, ok := d.reduce(n, den)
	if !ok {
		return Value{}, 0, nonCanonical(NonCanonicalReasonRationalNotReduced)
	}
	return v, after, nil
}

// readPart reads a rational's numerator or denominator, refused when wider than MaxRationalBits: a bignum's width is
// checked from its length, before it is built and before any gcd, so a wide part costs no more than reading it.
func (d *decoder) readPart(pos int) (*big.Int, int, error) {
	h, err := d.readHead(pos)
	if err != nil {
		return nil, 0, err
	}
	switch {
	case h.major == majorUnsigned:
		return new(big.Int).SetUint64(h.argument), h.end, nil
	case h.major == majorNegative:
		n := new(big.Int).SetUint64(h.argument)
		return n.Not(n), h.end, nil
	case h.major == majorTag && (h.argument == tagPositiveBignum || h.argument == tagNegativeBignum):
		return d.readBignum(pos, h.argument, h.end, MaxRationalBits, rationalTooWide)
	}
	return nil, 0, CanonicalErrorNonCanonical{Offset: uint64(pos), Reason: NonCanonicalReasonRationalNotInteger}
}

// span is n bytes from pos, and the offset after them: UnexpectedEnd when fewer are left.
func (d *decoder) span(pos int, n uint64) ([]byte, int, error) {
	available := uint64(len(d.input) - min(pos, len(d.input)))
	if n > available {
		return nil, 0, CanonicalErrorUnexpectedEnd{Offset: uint64(pos)}
	}
	end := pos + int(n)
	return d.input[pos:end], end, nil
}

// count checks an array's or map's count against the input left: every element needs at least one byte (two per map
// entry), so a count beyond the remaining input is truncated, not allocated.
func (d *decoder) count(pos int, n uint64, perEntry uint64) (uint64, error) {
	available := uint64(len(d.input) - min(pos, len(d.input)))
	if n > available/perEntry {
		return 0, CanonicalErrorUnexpectedEnd{Offset: uint64(pos)}
	}
	return n, nil
}
