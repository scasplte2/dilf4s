package core

import "math"

// ToHex is the lowercase hexadecimal of b; TooLarge (with the longest string a Go int indexes) when it would be longer
// than that, which a byte string of half of it is on a 32-bit int.
func ToHex(b []byte) (string, error) {
	if len(b) > math.MaxInt/2 {
		return "", TooLarge{Limit: math.MaxInt}
	}
	return toHex(b), nil
}

// toHex is the lowercase hexadecimal of b, which is short enough for it.
func toHex(b []byte) string {
	const digits = "0123456789abcdef"
	out := make([]byte, 2*len(b))
	for i, x := range b {
		out[2*i], out[2*i+1] = digits[x>>4], digits[x&0x0f]
	}
	return string(out)
}

// FromHex is the bytes of hexadecimal text: ASCII digits and a-f in either case, two per byte. Refused as
// HashErrorInvalidHex at the first byte that is not a hex digit, or else, for an odd number of digits, at the text's
// length: every byte before either is ASCII, so the offset is the same in UTF-8 bytes, UTF-16 units and code points
// (docs/specs/canonical-encoding.md section 4).
func FromHex(text string) ([]byte, error) { return hexBytes(text, 0) }

// hexBytes is FromHex, its offsets counted from `from` (a digest's 0x).
func hexBytes(text string, from int) ([]byte, error) {
	for i := 0; i < len(text); i++ {
		if _, ok := hexDigit(text[i]); !ok {
			return nil, HashErrorInvalidHex{Offset: uint64(from + i)}
		}
	}
	if len(text)%2 != 0 {
		return nil, HashErrorInvalidHex{Offset: uint64(from + len(text))}
	}
	out := make([]byte, len(text)/2)
	for i := range out {
		h, _ := hexDigit(text[2*i])
		l, _ := hexDigit(text[2*i+1])
		out[i] = h<<4 | l
	}
	return out, nil
}

// hexDigit is the value of an ASCII hex digit, either case.
func hexDigit(h byte) (byte, bool) {
	switch {
	case h >= '0' && h <= '9':
		return h - '0', true
	case h >= 'a' && h <= 'f':
		return h - 'a' + 10, true
	case h >= 'A' && h <= 'F':
		return h - 'A' + 10, true
	}
	return 0, false
}
