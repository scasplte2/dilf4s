package core

import (
	"math/big"
)

// Rational is a rational n/d with d >= 2, gcd(|n|, d) = 1 and both within MaxRationalBits: never an integer. The zero
// Rational is no rational (its denominator is 0): only Value.AsRational gives a real one.
type Rational struct {
	num Int
	den Int
}

// Numerator is a copy of the numerator, carrying the sign.
func (r Rational) Numerator() *big.Int { return r.num.Big() }

// Denominator is a copy of the denominator, at least 2.
func (r Rational) Denominator() *big.Int { return r.den.Big() }

// NumeratorBitLength is the numerator's Java bitLength. O(1).
func (r Rational) NumeratorBitLength() uint64 { return r.num.bits }

// DenominatorBitLength is the denominator's Java bitLength. O(1).
func (r Rational) DenominatorBitLength() uint64 { return r.den.bits }

// DivisionByZero is the refusal of Number with a zero denominator: no canonical value is that number.
type DivisionByZero struct{}

func (DivisionByZero) Error() string { return "division by zero" }

// rationalValue is the rational n/d, which the caller has checked is reduced and not an integer; it checks the width
// itself, so no wider rational exists.
func rationalValue(n, d *big.Int) (Value, bool) {
	nb, db := bitLength(n), bitLength(d)
	if nb > MaxRationalBits || db > MaxRationalBits {
		return Value{}, false
	}
	weight := addWeight(addWeight(2, nb/64), db/64)
	return Value{&node{kind: KindRational, num: Int{v: n, bits: nb}, den: Int{v: d, bits: db}, weight: weight}}, true
}

// Number is the number numerator / denominator in the single numeric domain: an integer when it is one (refused as
// CanonicalErrorIntegerTooWide past MaxIntegerBits), otherwise a reduced rational (refused as
// CanonicalErrorRationalTooWide when its numerator or denominator is wider than MaxRationalBits), checked after it is
// reduced. A zero denominator is DivisionByZero. Neither argument is kept.
func Number(numerator, denominator *big.Int) (Value, error) {
	n, d := new(big.Int), new(big.Int)
	if numerator != nil {
		n.Set(numerator)
	}
	if denominator != nil {
		d.Set(denominator)
	}
	if d.Sign() == 0 {
		return Value{}, DivisionByZero{}
	}
	g := new(big.Int).GCD(nil, nil, new(big.Int).Abs(n), new(big.Int).Abs(d))
	n.Quo(n, g)
	d.Quo(d, g)
	if d.Sign() < 0 {
		n.Neg(n)
		d.Neg(d)
	}
	if d.Cmp(big.NewInt(1)) == 0 {
		i, ok := newInt(n)
		if !ok {
			return Value{}, integerTooWide()
		}
		return IntValue(i), nil
	}
	if v, ok := rationalValue(n, d); ok {
		return v, nil
	}
	return Value{}, rationalTooWide()
}

// RationalOf is the rational numerator / denominator exactly as given, if it already satisfies the invariants (as a
// decoder must check): denominator >= 2, both within MaxRationalBits (checked first, so a wide one runs no gcd), and
// in lowest terms. Neither argument is kept.
func RationalOf(numerator, denominator *big.Int) (Value, bool) {
	if numerator == nil || denominator == nil {
		return Value{}, false
	}
	return exactRational(new(big.Int).Set(numerator), new(big.Int).Set(denominator))
}

// exactRational is RationalOf of parts this package holds alone.
func exactRational(n, d *big.Int) (Value, bool) {
	if d.Cmp(big.NewInt(2)) < 0 || bitLength(n) > MaxRationalBits || bitLength(d) > MaxRationalBits {
		return Value{}, false
	}
	g := new(big.Int).GCD(nil, nil, new(big.Int).Abs(n), d)
	if g.Cmp(big.NewInt(1)) != 0 {
		return Value{}, false
	}
	return rationalValue(n, d)
}
