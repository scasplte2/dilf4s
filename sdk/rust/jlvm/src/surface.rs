//! The JSON authoring surface (`Parser.scala`, `JsonSurface.scala`, docs/specs/jlvm.md section 1).
//!
//! Text is validated by the strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
//! structurally with nulls and member order still visible, so a null field cannot turn a map expression into an
//! operation. Scalars and literal values are converted by the canonical parser itself, from their source text.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::{CanonicalError, Value, json};

use crate::ast::{Expr, Program, VarRef, canonical_error};
use crate::error::JlvmError;
use crate::op::{Op, call_name, removed};
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

impl Json<'_> {
    /// The text of a JSON string.
    fn as_str(&self) -> Option<String> {
        match (&self.kind, json::parse(self.span)) {
            (Kind::Str, Ok(Value::Text(s))) => Some(String::from(s.as_str())),
            _ => None,
        }
    }

    /// The canonical value of this JSON, with `0x`-hex strings as bytes (`JsonSurface.value`).
    fn value(&self) -> Result<Value, JlvmError> {
        json::parse(self.span).map(from_canonical_json).map_err(|e| JlvmError::malformed(alloc::format!("{e}")))
    }
}

/// The structure of `text`, which the strict parser has accepted: an explicit stack, no recursion.
fn tree(text: &str) -> Option<Json<'_>> {
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
                    match json::parse(span) {
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
    strict(text, true)?;
    let j = tree(text).ok_or_else(|| JlvmError::malformed("not JSON"))?;
    let expr = from_json(&j)?;
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
fn from_json(j: &Json<'_>) -> Result<Expr, JlvmError> {
    match &j.kind {
        Kind::Null => Ok(Expr::Lit(Value::Null)),
        Kind::Scalar | Kind::Str => j.value().map(Expr::Lit),
        Kind::Arr(items) => array(items),
        Kind::Obj(fields) => match fields.as_slice() {
            [(key, arg)] => single(key, arg),
            _ => map_expr(fields.iter().map(|(k, x)| (k, x))),
        },
    }
}

/// An array headed by a call's name is rejected, so text shaped like a call is never read as data by accident.
fn array(items: &[Json<'_>]) -> Result<Expr, JlvmError> {
    match items.first().and_then(Json::as_str).filter(|s| call_name(s)) {
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
        None => elements(items),
    }
}

fn elements(items: &[Json<'_>]) -> Result<Expr, JlvmError> {
    items.iter().map(from_json).collect::<Result<Vec<_>, _>>().map(Expr::Arr)
}

/// A `let`'s `[name, expression]` pairs: a name is never code, so a pair is exempt from the array check.
fn let_bindings(j: &Json<'_>) -> Result<Expr, JlvmError> {
    match &j.kind {
        Kind::Arr(pairs) => pairs
            .iter()
            .map(|pair| match &pair.kind {
                Kind::Arr(items) => elements(items),
                _ => from_json(pair),
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Expr::Arr),
        _ => from_json(j),
    }
}

/// A map expression: fields kept in code-point order of their (distinct) keys.
fn map_expr<'a, 't: 'a>(fields: impl Iterator<Item = (&'a String, &'a Json<'t>)>) -> Result<Expr, JlvmError> {
    let mut out: Vec<(String, Expr)> =
        fields.map(|(k, x)| from_json(x).map(|e| (k.clone(), e))).collect::<Result<_, _>>()?;
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(Expr::Obj(out))
}

fn single(key: &String, arg: &Json<'_>) -> Result<Expr, JlvmError> {
    match key.as_str() {
        "literal" => arg.value().map(Expr::Lit),
        "var" => variable(arg),
        "object" => match &arg.kind {
            Kind::Obj(fields) => map_expr(fields.iter().map(|(k, x)| (k, x))),
            _ => malformed("object takes an object: {\"object\": {key: expression, ...}}"),
        },
        data if data.starts_with('_') => map_expr(core::iter::once((key, arg))),
        name => match (removed(name), Op::from_wire(name)) {
            (Some(hint), _) => malformed(alloc::format!("\"{name}\" is not an operation: {hint}")),
            (None, Some(op)) => operation(op, arg),
            (None, None) => malformed(alloc::format!(
                "unknown operation \"{name}\": write single-key data as {{\"object\": {{...}}}} or {{\"literal\": {{...}}}}"
            )),
        },
    }
}

fn operation(op: Op, arg: &Json<'_>) -> Result<Expr, JlvmError> {
    match &arg.kind {
        Kind::Arr(items) => match items.as_slice() {
            [bindings, body] if op == Op::Let => {
                let b = let_bindings(bindings)?;
                let x = from_json(body)?;
                Ok(Expr::Apply(op, alloc::vec![b, x]))
            }
            _ => items.iter().map(from_json).collect::<Result<Vec<_>, _>>().map(|xs| Expr::Apply(op, xs)),
        },
        _ if op.is_variadic() => from_json(arg).map(|x| Expr::Spread(op, Box::new(x))),
        _ => from_json(arg).map(|x| Expr::Apply(op, alloc::vec![x])),
    }
}

/// A path is text or an integer; an object or an array is an expression computing the path.
fn path(j: &Json<'_>, default: Option<Value>) -> Result<Expr, JlvmError> {
    let text = match (&j.kind, json::parse(j.span)) {
        (Kind::Str, Ok(Value::Text(s))) => Some(String::from(s.as_str())),
        (Kind::Scalar, Ok(Value::Int(i))) => Some(alloc::string::ToString::to_string(&i)),
        _ => None,
    };
    if let Some(t) = text {
        return VarRef::parse(&t).map(|r| Expr::Var(r, default));
    }
    match &j.kind {
        Kind::Obj(_) => from_json(j).map(|p| Expr::VarAt(Box::new(p), default)),
        // a literal array here is the path's segments, not code, so it is exempt from the array check
        Kind::Arr(items) => elements(items).map(|p| Expr::VarAt(Box::new(p), default)),
        _ => malformed("a var path is text, an integer, or an expression computing one"),
    }
}

fn variable(arg: &Json<'_>) -> Result<Expr, JlvmError> {
    match &arg.kind {
        Kind::Arr(items) => match items.as_slice() {
            [p] => path(p, None),
            [p, d] => {
                let d = default_value(d)?;
                path(p, Some(d).filter(|v| *v != Value::Null))
            }
            _ => malformed("var takes a path, or [path, default]"),
        },
        _ => path(arg, None),
    }
}

/// A default is a literal value, never evaluated; an object or array default is quoted, `{"literal": ...}`.
fn default_value(j: &Json<'_>) -> Result<Value, JlvmError> {
    match &j.kind {
        Kind::Obj(fields) => match fields.as_slice() {
            [(k, x)] if k == "literal" => x.value(),
            _ => {
                malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
            }
        },
        Kind::Arr(_) => {
            malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
        }
        _ => j.value(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trees_keep_nulls_and_order() {
        let t = tree(r#"{"b": null, "a": [1, "x\"y", {}], "c": true}"#).map(|j| match j.kind {
            Kind::Obj(fs) => fs.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>(),
            _ => Vec::new(),
        });
        assert_eq!(t, Some(alloc::vec![String::from("b"), String::from("a"), String::from("c")]));
        assert!(tree("[]").is_some_and(|j| matches!(j.kind, Kind::Arr(_))));
        assert!(tree("{}").is_some_and(|j| matches!(j.kind, Kind::Obj(_))));
    }

    #[test]
    fn null_fields_keep_a_map_expression() {
        // two keys, one null: a map expression, not the operation +
        let p = parse(r#"{"+": [1, 2], "x": null}"#).map(|p| p.expr);
        assert!(matches!(p, Ok(Expr::Obj(fs)) if fs.len() == 2));
    }
}
