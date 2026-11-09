package core

import (
	"strconv"
	"strings"
	"unicode/utf8"
)

// MaxPreviewCodePoints is the most code points a Preview keeps.
const MaxPreviewCodePoints = 32

// Preview is caller text as an error may hold it (docs/specs/conformance.md section 8): its first
// MaxPreviewCodePoints code points, each ill-formed sequence (an unpaired surrogate's included) replaced by U+FFFD, and
// its whole length in bytes. Its fields are unexported and PreviewOf truncates, so an error holding a preview holds at
// most that much of what the caller sent, however long the text was. The zero Preview is the preview of "".
type Preview struct {
	head   string
	length int
}

// PreviewOf is the preview of text: O(MaxPreviewCodePoints), its length read, not measured. Its head is a fresh copy,
// never a view of text.
func PreviewOf(text string) Preview { return previewOfWTF8(text) }

// previewOfWTF8 is the preview of generalised UTF-8 (WTF-8), where an unpaired surrogate is its 3-byte form: the JSON
// parser's member names before they are checked. Any other ill-formed sequence also previews as U+FFFD: each
// sequence is as wide as its lead byte says (2 for 0xc0..0xdf, 3 for 0xe0..0xef, 4 for 0xf0..0xf7, else 1), as the
// Rust SDK reads it, and is U+FFFD unless it is one well-formed code point.
func previewOfWTF8(text string) Preview {
	var head strings.Builder
	at := 0
	for range MaxPreviewCodePoints {
		if at >= len(text) {
			break
		}
		width := 1
		switch lead := text[at]; {
		case lead >= 0xc0 && lead <= 0xdf:
			width = 2
		case lead >= 0xe0 && lead <= 0xef:
			width = 3
		case lead >= 0xf0 && lead <= 0xf7:
			width = 4
		}
		end := min(at+width, len(text))
		unit := text[at:end]
		at = end
		r, size := utf8.DecodeRuneInString(unit)
		if r == utf8.RuneError || size != len(unit) {
			r = utf8.RuneError
		}
		head.WriteRune(r)
	}
	return Preview{head: head.String(), length: len(text)}
}

// Head is the first MaxPreviewCodePoints code points.
func (p Preview) Head() string { return p.head }

// Length is the whole text's length in UTF-8 bytes.
func (p Preview) Length() int { return p.length }

// Truncated reports whether the text was longer than Head.
func (p Preview) Truncated() bool { return len(p.head) < p.length }

// String is the head quoted, "…" when truncated, and the length: "abc" (UTF-8 length 3).
func (p Preview) String() string {
	var b strings.Builder
	b.WriteByte('"')
	escapeHead(&b, p.head)
	b.WriteByte('"')
	if p.Truncated() {
		b.WriteString("…")
	}
	b.WriteString(" (UTF-8 length ")
	b.WriteString(strconv.Itoa(p.length))
	b.WriteByte(')')
	return b.String()
}

// Short is the head with JSON's escapes, then "…" when truncated: a path segment's form.
func (p Preview) Short() string {
	var b strings.Builder
	escapeHead(&b, p.head)
	if p.Truncated() {
		b.WriteString("…")
	}
	return b.String()
}

func escapeHead(b *strings.Builder, head string) {
	const digits = "0123456789abcdef"
	for _, r := range head {
		switch {
		case r == '"':
			b.WriteString(`\"`)
		case r == '\\':
			b.WriteString(`\\`)
		case r < ' ' || r == 0x7f:
			b.WriteString(`\u00`)
			b.WriteByte(digits[r>>4])
			b.WriteByte(digits[r&0xf])
		default:
			b.WriteRune(r)
		}
	}
}
