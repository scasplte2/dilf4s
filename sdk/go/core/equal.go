package core

import (
	"strconv"
	"strings"
)

// Equality is on content (the canonical encoding is the identity), and costs O(the distinct pairs of shared payloads
// compared), never O(weight): a comparison keeps the pairs of containers, and of large texts, byte strings and numbers,
// that it has proven equal, and skips them when they recur. A node's identity is only ever a fast path: the same node
// is the same value, and a pair is recorded only after its contents compared equal.

// largeString is the weight from which a text or byte string is memoised; largeNumber, the weight from which a number
// is (limbs: eight bytes each).
const (
	largeString = 64
	largeNumber = 8
)

type nodePair struct{ a, b *node }

// equalFrame is two containers whose elements are being compared.
type equalFrame struct {
	a, b *node
	next int
}

// comparison is one comparison, with an explicit stack (at most MaxDepth frames) and the pairs proven equal so far.
// steps counts the scalar and container pairs compared, for the tests' work counts.
type comparison struct {
	stack  []equalFrame
	proven map[nodePair]struct{}
	steps  int
}

// Equal reports whether v and w are the same canonical value.
func (v Value) Equal(w Value) bool {
	var c comparison
	return c.equal(v, w)
}

func (c *comparison) equal(v, w Value) bool {
	if !c.value(v, w) {
		return false
	}
	for len(c.stack) > 0 {
		top := &c.stack[len(c.stack)-1]
		count := len(top.a.items)
		if top.a.kind == KindMap {
			count = len(top.a.keys)
		}
		if top.next >= count {
			c.prove(nodePair{top.a, top.b})
			c.stack = c.stack[:len(c.stack)-1]
			continue
		}
		i := top.next
		top.next++
		var x, y Value
		if top.a.kind == KindMap {
			if top.a.keys[i] != top.b.keys[i] {
				return false
			}
			x, y = top.a.vals[i], top.b.vals[i]
		} else {
			x, y = top.a.items[i], top.b.items[i]
		}
		if !c.value(x, y) {
			return false
		}
	}
	return true
}

func (c *comparison) prove(p nodePair) {
	if c.proven == nil {
		c.proven = map[nodePair]struct{}{}
	}
	c.proven[p] = struct{}{}
}

func (c *comparison) isProven(p nodePair) bool {
	_, ok := c.proven[p]
	return ok
}

// value is false when v and w differ; true when they are equal scalars, or containers now open on the stack.
func (c *comparison) value(v, w Value) bool {
	c.steps++
	if v.Kind() != w.Kind() || v.Weight() != w.Weight() || v.Depth() != w.Depth() {
		return false
	}
	a, b := v.n, w.n
	if a == b || v.Kind() == KindNull {
		return true
	}
	pair := nodePair{a, b}
	switch v.Kind() {
	case KindBool:
		return a.b == b.b
	case KindInt:
		return c.scalar(pair, a.weight >= largeNumber, func() bool { return a.num.Equal(b.num) })
	case KindRational:
		return c.scalar(pair, a.weight >= largeNumber, func() bool { return a.num.Equal(b.num) && a.den.Equal(b.den) })
	case KindText:
		return c.scalar(pair, a.weight >= largeString, func() bool { return a.text == b.text })
	case KindBytes:
		return c.scalar(pair, a.weight >= largeString, func() bool { return string(a.bytes) == string(b.bytes) })
	case KindArray:
		if len(a.items) != len(b.items) {
			return false
		}
	case KindMap:
		if len(a.keys) != len(b.keys) {
			return false
		}
	}
	if !c.isProven(pair) {
		c.stack = append(c.stack, equalFrame{a: a, b: b})
	}
	return true
}

// scalar compares two scalars of one kind: a large pair already proven equal, or compared now (and a large pair
// recorded when equal).
func (c *comparison) scalar(pair nodePair, large bool, equal func() bool) bool {
	if large && c.isProven(pair) {
		return true
	}
	same := equal()
	if same && large {
		c.prove(pair)
	}
	return same
}

// DebugBudget is how much of a value String shows, in units of Value.Weight; the rest is elided as "…". Formatting any
// value is then bounded by this budget, however often it repeats a shared part.
const DebugBudget = 1 << 16

// DebugDepth is how many arrays and maps deep String shows; deeper ones are elided as "…".
const DebugDepth = 16

// String is a diagnostic rendering of v, bounded: at most DebugBudget units of weight and DebugDepth levels are shown,
// the rest elided as "…". Text is quoted with Go's escapes; numbers past largeNumber limbs are shown by their bit
// length, never their digits. Walked with an explicit stack.
func (v Value) String() string {
	var b strings.Builder
	left := uint64(DebugBudget)
	type frame struct {
		n    *node
		next int
	}
	var stack []frame
	show := func(x Value) {
		if x.Kind() == KindArray || x.Kind() == KindMap {
			if left == 0 || len(stack) >= DebugDepth {
				b.WriteString("…")
				return
			}
			left--
			if x.Kind() == KindArray {
				b.WriteByte('[')
			} else {
				b.WriteByte('{')
			}
			stack = append(stack, frame{n: x.n})
			return
		}
		if x.Weight() > left {
			left = 0
			b.WriteString("…")
			return
		}
		left -= x.Weight()
		switch x.Kind() {
		case KindNull:
			b.WriteString("null")
		case KindBool:
			b.WriteString(strconv.FormatBool(x.n.b))
		case KindInt:
			if x.n.weight >= largeNumber {
				b.WriteString("<an integer of " + strconv.FormatUint(x.n.num.bits, 10) + " bits>")
			} else {
				b.WriteString(x.n.num.String())
			}
		case KindRational:
			if x.n.weight >= largeNumber {
				b.WriteString("<a rational of " + strconv.FormatUint(x.n.num.bits, 10) + "/" + strconv.FormatUint(x.n.den.bits, 10) + " bits>")
			} else {
				b.WriteString(x.n.num.String() + "/" + x.n.den.String())
			}
		case KindText:
			b.WriteString(strconv.Quote(x.n.text))
		case KindBytes:
			// within the budget, so short
			b.WriteString("h'" + toHex(x.n.bytes) + "'")
		}
	}
	show(v)
	for len(stack) > 0 {
		top := &stack[len(stack)-1]
		count := len(top.n.items)
		if top.n.kind == KindMap {
			count = len(top.n.keys)
		}
		if top.next >= count {
			if top.n.kind == KindArray {
				b.WriteByte(']')
			} else {
				b.WriteByte('}')
			}
			stack = stack[:len(stack)-1]
			continue
		}
		if top.next > 0 {
			b.WriteString(", ")
		}
		if left == 0 {
			b.WriteString("…")
			top.next = count
			continue
		}
		i := top.next
		top.next++
		if top.n.kind == KindMap {
			k := top.n.keys[i]
			if sizedWeight(len(k)) > left {
				left = 0
				b.WriteString("…")
				top.next = count
				continue
			}
			left -= sizedWeight(len(k))
			b.WriteString(strconv.Quote(k) + ": ")
			show(top.n.vals[i])
		} else {
			show(top.n.items[i])
		}
	}
	return b.String()
}
