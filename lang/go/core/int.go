package core

import (
	"math/big"
)

// MaxIntegerBits is the widest Int, in Java BigInteger.bitLength bits (docs/specs/canonical-encoding.md section 1):
// the widest whose every integer has at most 10,000 digits, so every integer renders as JSON and reads back.
const MaxIntegerBits = CanonicalMaxIntegerBits

// MaxRationalBits is the widest numerator or denominator of a Rational: the integer bound, so a decoder runs at most
// one gcd of operands this wide per rational.
const MaxRationalBits = CanonicalMaxRationalBits

// Int is an immutable integer within MaxIntegerBits. The zero Int is 0. It never shares memory with a caller: IntFromBig
// copies its argument, and Big hands out a copy.
type Int struct {
	// nil is 0; never handed out, never changed after construction
	v    *big.Int
	bits uint64
}

// bitLength is Java's BigInteger.bitLength (the reference's): the bits of the minimal two's-complement form without
// its sign bit, which for a negative n is the bit length of |n| - 1, one less than |n|'s exactly when |n| is a power
// of two (-2^63 has 63 bits).
func bitLength(n *big.Int) uint64 {
	if n == nil || n.Sign() == 0 {
		return 0
	}
	bits := uint64(n.BitLen())
	if n.Sign() < 0 && uint64(n.TrailingZeroBits()) == bits-1 {
		return bits - 1
	}
	return bits
}

// newInt holds n, which this package built and gives to no one, after checking its width.
func newInt(n *big.Int) (Int, bool) {
	bits := bitLength(n)
	if bits > MaxIntegerBits {
		return Int{}, false
	}
	return Int{v: n, bits: bits}, true
}

// IntOf is the integer n: every int64 is within MaxIntegerBits.
func IntOf(n int64) Int {
	b := new(big.Int).SetInt64(n)
	return Int{v: b, bits: bitLength(b)}
}

// UintOf is the integer n: every uint64 is within MaxIntegerBits.
func UintOf(n uint64) Int {
	b := new(big.Int).SetUint64(n)
	return Int{v: b, bits: bitLength(b)}
}

// IntFromBig is a copy of n, or CanonicalErrorIntegerTooWide when it is wider than MaxIntegerBits. A nil n is 0.
func IntFromBig(n *big.Int) (Int, error) {
	if n == nil {
		return Int{}, nil
	}
	if i, ok := newInt(new(big.Int).Set(n)); ok {
		return i, nil
	}
	return Int{}, integerTooWide()
}

func integerTooWide() error { return CanonicalErrorIntegerTooWide{Limit: MaxIntegerBits} }

func rationalTooWide() error { return CanonicalErrorRationalTooWide{Limit: MaxRationalBits} }

// Big is a copy of the integer.
func (i Int) Big() *big.Int {
	if i.v == nil {
		return new(big.Int)
	}
	return new(big.Int).Set(i.v)
}

// BitLength is the integer's Java bitLength. O(1): computed when the integer was made.
func (i Int) BitLength() uint64 { return i.bits }

// Sign is -1, 0 or 1.
func (i Int) Sign() int {
	if i.v == nil {
		return 0
	}
	return i.v.Sign()
}

// Cmp compares i with j: -1, 0 or 1.
func (i Int) Cmp(j Int) int { return i.big().Cmp(j.big()) }

// Equal reports whether i and j are the same integer.
func (i Int) Equal(j Int) bool { return i.bits == j.bits && i.Cmp(j) == 0 }

// Int64 is the integer as an int64, when it fits.
func (i Int) Int64() (int64, bool) {
	if !i.big().IsInt64() {
		return 0, false
	}
	return i.big().Int64(), true
}

// Uint64 is the integer as a uint64, when it fits.
func (i Int) Uint64() (uint64, bool) {
	if !i.big().IsUint64() {
		return 0, false
	}
	return i.big().Uint64(), true
}

// String is the integer's decimal: at most 10,000 digits, as every integer is within MaxIntegerBits.
func (i Int) String() string { return i.big().String() }

// weight is 1 + bitLength / 64 (see Value.Weight).
func (i Int) weight() uint64 { return addWeight(1, i.bits/64) }

// big is the integer, for this package to read and never to change or hand out.
func (i Int) big() *big.Int {
	if i.v == nil {
		return zero()
	}
	return i.v
}

// zero is a fresh 0: a constant would be package state.
func zero() *big.Int { return new(big.Int) }

// BigBound is a bound of an integer range, as a sign and a big-endian magnitude, so that generated code can write it
// as a constant.
type BigBound struct {
	Negative  bool
	Magnitude string
}

// Int is the bound as an integer; 0 for a bound wider than MaxIntegerBits, which no generated code writes.
func (b BigBound) Int() Int {
	i, _ := newInt(b.value())
	return i
}

func (b BigBound) value() *big.Int {
	n := new(big.Int).SetBytes([]byte(b.Magnitude))
	if b.Negative {
		n.Neg(n)
	}
	return n
}
