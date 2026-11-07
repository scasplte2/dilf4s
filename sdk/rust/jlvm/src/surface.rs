//! The JSON authoring surface (`Parser.scala`, `JsonSurface.scala`, docs/specs/jlvm.md section 1).
//!
//! Text is validated by the strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
//! structurally with nulls and member order still visible, so a null field cannot turn a map expression into an
//! operation. Scalars and literal values are converted by the canonical parser itself, from their source text: where a
//! string or an integer is looked for, only a string's or a scalar's span is read, so no span is read once per level of
//! the containers around it ([`Spans`] counts what is read).

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::Cell;

use dilf4s_core::{CanonicalError, Preview, Value, json};

use crate::ast::{Expr, Program, VarRef, canonical_error};
use crate::error::{JlvmError, Raise};
use crate::op::{Op, OpClass, call_name, removed};
use crate::values::from_canonical_json;

/// A JSON node of text the strict parser has accepted, with its source span.
pub(crate) struct Json<'t> {
    span: &'t str,
    kind: Kind<'t>,
}

enum Kind<'t> {
    Null,
    Scalar,
    Str,
    Arr(Vec<Json<'t>>),
    Obj(Vec<(String, Json<'t>)>),
}

/// The spans read back through the strict parser, by total length (`Steps.spanned` in the reference).
#[derive(Default)]
pub(crate) struct Spans(Cell<usize>);

impl Spans {
    fn read(&self, span: &str) -> Result<Value, CanonicalError> {
        self.0.set(self.0.get().saturating_add(span.len()));
        json::parse(span)
    }
}

impl Json<'_> {
    /// The text of a JSON string; nothing is read for any other node.
    fn as_str(&self, r: &Spans) -> Option<String> {
        match self.kind {
            Kind::Str => match r.read(self.span) {
                Ok(Value::Text(s)) => Some(String::from(s.as_str())),
                _ => None,
            },
            _ => None,
        }
    }

    /// The integer a number denotes, as path text; nothing is read for any other node.
    fn as_integer(&self, r: &Spans) -> Option<String> {
        match self.kind {
            Kind::Scalar => match r.read(self.span) {
                Ok(Value::Int(i)) => Some(alloc::string::ToString::to_string(&i)),
                _ => None,
            },
            _ => None,
        }
    }

    /// The canonical value of this JSON, with `0x`-hex strings as bytes (`JsonSurface.value`).
    fn value(&self, r: &Spans) -> Result<Value, JlvmError> {
        r.read(self.span).map(from_canonical_json).map_err(|e| JlvmError::malformed(alloc::format!("{e}")))
    }
}

/// The structure of `text`, which the strict parser has accepted: an explicit stack, no recursion.
fn tree<'t>(text: &'t str, r: &Spans) -> Option<Json<'t>> {
    enum Open<'t> {
        Arr(usize, Vec<Json<'t>>),
        Obj(usize, Vec<(String, Json<'t>)>, Option<String>),
    }
    let bytes = text.as_bytes();
    let at = |i: usize| bytes.get(i).copied();
    let mut stack: Vec<Open<'_>> = Vec::new();
    let mut i = 0usize;
    loop {
        while matches!(at(i), Some(b' ' | b'\t' | b'\n' | b'\r' | b',' | b':')) {
            i = i.saturating_add(1);
        }
        let start = i;
        let node = match at(i)? {
            b'{' => {
                stack.push(Open::Obj(i, Vec::new(), None));
                i = i.saturating_add(1);
                continue;
            }
            b'[' => {
                stack.push(Open::Arr(i, Vec::new()));
                i = i.saturating_add(1);
                continue;
            }
            b'}' | b']' => {
                i = i.saturating_add(1);
                match stack.pop()? {
                    Open::Arr(s, items) => Json { span: text.get(s..i)?, kind: Kind::Arr(items) },
                    Open::Obj(s, fields, _) => Json { span: text.get(s..i)?, kind: Kind::Obj(fields) },
                }
            }
            b'"' => {
                i = i.saturating_add(1);
                loop {
                    match at(i)? {
                        b'"' => break,
                        b'\\' => i = i.saturating_add(2),
                        _ => i = i.saturating_add(1),
                    }
                }
                i = i.saturating_add(1);
                let span = text.get(start..i)?;
                // in an object awaiting a key, a string is the key
                if let Some(Open::Obj(_, _, key @ None)) = stack.last_mut() {
                    match r.read(span) {
                        Ok(Value::Text(k)) => *key = Some(String::from(k.as_str())),
                        _ => return None,
                    }
                    continue;
                }
                Json { span, kind: Kind::Str }
            }
            first => {
                while !matches!(at(i), None | Some(b' ' | b'\t' | b'\n' | b'\r' | b',' | b']' | b'}')) {
                    i = i.saturating_add(1);
                }
                Json { span: text.get(start..i)?, kind: if first == b'n' { Kind::Null } else { Kind::Scalar } }
            }
        };
        match stack.last_mut() {
            None => return Some(node),
            Some(Open::Arr(_, items)) => items.push(node),
            Some(Open::Obj(_, fields, key)) => fields.push((key.take()?, node)),
        }
    }
}

/// Program or data text through the strict parser, with the JLVM's error kinds: a depth bound is `Limit` for a
/// program and `Malformed` for data.
fn strict(text: &str, depth_is_limit: bool) -> Result<Value, JlvmError> {
    json::parse(text).map_err(|e| match e {
        e @ CanonicalError::DepthExceeded { .. } if depth_is_limit => canonical_error(e),
        other => JlvmError::malformed(alloc::format!("{other}")),
    })
}

/// Parses a program from its JSON surface text (`Parser.parse`).
pub fn parse(text: &str) -> Result<Program, JlvmError> {
    parse_counted(text, &Spans::default())
}

/// [`parse`], telling `r` of each span read back.
pub(crate) fn parse_counted(text: &str, r: &Spans) -> Result<Program, JlvmError> {
    strict(text, true)?;
    let j = tree(text, r).ok_or_else(|| JlvmError::malformed("not JSON"))?;
    let expr = from_json(&j, r)?;
    drop(j);
    Program::from(expr)
}

/// Parses input data or a host binding from JSON surface text (`JsonSurface.parse`): the strict parser, then `0x`-hex
/// strings as bytes.
pub fn parse_value(text: &str) -> Result<Value, JlvmError> {
    strict(text, false).map(from_canonical_json)
}

fn malformed<A>(m: impl Into<String>) -> Result<A, JlvmError> {
    Err(JlvmError::malformed(m))
}

/// Recursion is bounded by the strict parser's depth bound (256), which the text has passed.
fn from_json(j: &Json<'_>, r: &Spans) -> Result<Expr, JlvmError> {
    match &j.kind {
        Kind::Null => Ok(Expr::Lit(Value::Null)),
        Kind::Scalar | Kind::Str => j.value(r).map(Expr::Lit),
        Kind::Arr(items) => array(items, r),
        Kind::Obj(fields) => match fields.as_slice() {
            [(key, arg)] => single(key, arg, r),
            _ => map_expr(fields.iter().map(|(k, x)| (k, x)), r),
        },
    }
}

/// An array headed by a call's name is rejected, so text shaped like a call is never read as data by accident.
fn array(items: &[Json<'_>], r: &Spans) -> Result<Expr, JlvmError> {
    match items.first().and_then(|j| j.as_str(r)).filter(|s| call_name(s)) {
        Some(name) => {
            let call = removed(&name).map_or_else(
                || alloc::format!("write the call as {{\"{name}\": [...]}}"),
                |hint| alloc::format!("\"{name}\" is not an operation: {hint}"),
            );
            malformed(alloc::format!(
                "an array may not start with \"{name}\", which reads as a call: {call}; quote data as \
                 {{\"literal\": [...]}}, or start it with {{\"literal\": \"{name}\"}}"
            ))
        }
        None => elements(items, r),
    }
}

fn elements(items: &[Json<'_>], r: &Spans) -> Result<Expr, JlvmError> {
    items.iter().map(|j| from_json(j, r)).collect::<Result<Vec<_>, _>>().map(Expr::Arr)
}

/// A `let`'s `[name, expression]` pairs: a name is never code, so a pair is exempt from the array check.
fn let_bindings(j: &Json<'_>, r: &Spans) -> Result<Expr, JlvmError> {
    match &j.kind {
        Kind::Arr(pairs) => pairs
            .iter()
            .map(|pair| match &pair.kind {
                Kind::Arr(items) => elements(items, r),
                _ => from_json(pair, r),
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Expr::Arr),
        _ => from_json(j, r),
    }
}

/// A map expression: fields kept in code-point order of their (distinct) keys.
fn map_expr<'a, 't: 'a>(
    fields: impl Iterator<Item = (&'a String, &'a Json<'t>)>,
    r: &Spans,
) -> Result<Expr, JlvmError> {
    let mut out: Vec<(String, Expr)> =
        fields.map(|(k, x)| from_json(x, r).map(|e| (k.clone(), e))).collect::<Result<_, _>>()?;
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(Expr::Obj(out))
}

fn single(key: &String, arg: &Json<'_>, r: &Spans) -> Result<Expr, JlvmError> {
    match key.as_str() {
        "literal" => arg.value(r).map(Expr::Lit),
        "var" => variable(arg, r),
        "object" => match &arg.kind {
            Kind::Obj(fields) => map_expr(fields.iter().map(|(k, x)| (k, x)), r),
            _ => malformed("object takes an object: {\"object\": {key: expression, ...}}"),
        },
        data if data.starts_with('_') => map_expr(core::iter::once((key, arg)), r),
        name => match (removed(name), Op::from_wire(name)) {
            (Some(hint), _) => malformed(alloc::format!("\"{name}\" is not an operation: {hint}")),
            (None, Some(op)) => operation(op, arg, r),
            (None, None) => malformed(alloc::format!(
                "unknown operation {}: write single-key data as {{\"object\": {{...}}}} or {{\"literal\": {{...}}}}",
                Preview::of(name)
            )),
        },
    }
}

fn operation(op: Op, arg: &Json<'_>, r: &Spans) -> Result<Expr, JlvmError> {
    match &arg.kind {
        Kind::Arr(items) => match items.as_slice() {
            [bindings, body] if op == Op::Let => {
                let b = let_bindings(bindings, r)?;
                let x = from_json(body, r)?;
                Ok(Expr::Apply(op, alloc::vec![b, x]))
            }
            _ => items.iter().map(|j| from_json(j, r)).collect::<Result<Vec<_>, _>>().map(|xs| Expr::Apply(op, xs)),
        },
        _ if op.is_variadic() => from_json(arg, r).map(|x| Expr::Spread(op, Box::new(x))),
        _ => from_json(arg, r).map(|x| Expr::Apply(op, alloc::vec![x])),
    }
}

/// A path is text or an integer; an object or an array is an expression computing the path. Only a string's or a
/// number's span is read back, so a nested computed path is read once, not once per level.
fn path(j: &Json<'_>, default: Option<Value>, r: &Spans) -> Result<Expr, JlvmError> {
    if let Some(t) = j.as_str(r).or_else(|| j.as_integer(r)) {
        return VarRef::parse(&t).map(|r| Expr::Var(r, default));
    }
    match &j.kind {
        Kind::Obj(_) => from_json(j, r).map(|p| Expr::VarAt(Box::new(p), default)),
        // a literal array here is the path's segments, not code, so it is exempt from the array check
        Kind::Arr(items) => elements(items, r).map(|p| Expr::VarAt(Box::new(p), default)),
        _ => malformed("a var path is text, an integer, or an expression computing one"),
    }
}

fn variable(arg: &Json<'_>, r: &Spans) -> Result<Expr, JlvmError> {
    match &arg.kind {
        Kind::Arr(items) => match items.as_slice() {
            [p] => path(p, None, r),
            [p, d] => {
                let d = default_value(d, r)?;
                path(p, Some(d).filter(|v| *v != Value::Null), r)
            }
            _ => malformed("var takes a path, or [path, default]"),
        },
        _ => path(arg, None, r),
    }
}

/// A default is a literal value, never evaluated; an object or array default is quoted, `{"literal": ...}`.
fn default_value(j: &Json<'_>, r: &Spans) -> Result<Value, JlvmError> {
    match &j.kind {
        Kind::Obj(fields) => match fields.as_slice() {
            [(k, x)] if k == "literal" => x.value(r),
            _ => {
                malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
            }
        },
        Kind::Arr(_) => {
            malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
        }
        _ => j.value(r),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trees_keep_nulls_and_order() {
        let r = Spans::default();
        let t = tree(r#"{"b": null, "a": [1, "x\"y", {}], "c": true}"#, &r).map(|j| match j.kind {
            Kind::Obj(fs) => fs.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>(),
            _ => Vec::new(),
        });
        assert_eq!(t, Some(alloc::vec![String::from("b"), String::from("a"), String::from("c")]));
        assert!(tree("[]", &r).is_some_and(|j| matches!(j.kind, Kind::Arr(_))));
        assert!(tree("{}", &r).is_some_and(|j| matches!(j.kind, Kind::Obj(_))));
    }

    #[test]
    fn numbers_past_ten_thousand_digits_are_refused_as_literals_and_paths() {
        let most = "1".repeat(10_000);
        assert!(parse(&alloc::format!(r#"{{"var": {most}}}"#)).is_ok(), "a 10,000-digit path is read");
        let nines = "9".repeat(10_000);
        assert!(parse(&alloc::format!(r#"{{"+": [{nines}, 1]}}"#)).is_err(), "10,000 nines are past 33219 bits");
        assert!(parse(&alloc::format!(r#"{{"+": [{most}, 1]}}"#)).is_ok(), "and a 10,000-digit literal");
        assert!(parse(&alloc::format!(r#"{{"var": {most}9}}"#)).is_err(), "a 10,001-digit path is refused");
        assert!(parse(&alloc::format!(r#"{{"+": [{most}9, 1]}}"#)).is_err(), "and a 10,001-digit literal");
        let exponent = "1".repeat(10_001);
        assert!(parse(&alloc::format!(r#"{{"var": 1e{exponent}}}"#)).is_err(), "and a 10,001-digit exponent");
    }

    #[test]
    fn null_fields_keep_a_map_expression() {
        // two keys, one null: a map expression, not the operation +
        let p = parse(r#"{"+": [1, 2], "x": null}"#).map(|p| p.expr);
        assert!(matches!(p, Ok(Expr::Obj(fs)) if fs.len() == 2));
    }

    /// What reading `text` reads back. An array's leading string is read twice (once to check it names no call, once
    /// as its value) and every other span at most once, so this is at most twice the text however deep it nests.
    fn spans_read(text: &str) -> usize {
        let r = Spans::default();
        assert!(parse_counted(text, &r).is_ok(), "parses: {text:.80}");
        r.0.get()
    }

    #[test]
    fn spans_are_read_once_not_once_per_level() {
        let arrays = alloc::format!("{}1{}", "[".repeat(100), "]".repeat(100));
        let headed = alloc::format!("{}\"a\"{}", "[\"x\", ".repeat(100), "]".repeat(100));
        let paths = alloc::format!("{}\"a\"{}", "{\"var\": ".repeat(120), "}".repeat(120));
        let defaults = alloc::format!("{}\"a\"{}", "{\"var\": [".repeat(40), ", 1]}".repeat(40));
        for text in [&arrays, &headed, &paths, &defaults] {
            let read = spans_read(text);
            assert!(read <= 2 * text.len(), "{read} bytes read back for {} bytes of text", text.len());
        }
    }
}
