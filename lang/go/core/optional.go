package core

import "fmt"

// Optional is a value that may be absent: a model's optional field, or an error's. The zero Optional is absent. It
// holds its value by value, so it never aliases a caller's variable (as a pointer would), and reading it cannot
// dereference nil.
type Optional[T any] struct {
	value T
	ok    bool
}

// Some is the present value v.
func Some[T any](v T) Optional[T] { return Optional[T]{value: v, ok: true} }

// None is the absent value.
func None[T any]() Optional[T] { return Optional[T]{} }

// Get is the value and true when present, else the zero T and false.
func (o Optional[T]) Get() (T, bool) { return o.value, o.ok }

// IsSome reports whether the value is present.
func (o Optional[T]) IsSome() bool { return o.ok }

// OrElse is the value when present, else d.
func (o Optional[T]) OrElse(d T) T {
	if o.ok {
		return o.value
	}
	return d
}

// String is the value's own form when present, else "none".
func (o Optional[T]) String() string {
	if !o.ok {
		return "none"
	}
	return fmt.Sprint(o.value)
}
