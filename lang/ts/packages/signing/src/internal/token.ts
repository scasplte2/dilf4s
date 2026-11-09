/**
 * The construction token of keys and signatures. Their constructors take it, and this module is not exported from the
 * package, so only the scheme decoders and derivations in this package can build one: the TypeScript counterpart of
 * Scala's `private[crypto]` constructors.
 */
export const INTERNAL: unique symbol = Symbol("dilf4s-signing internal");
export type Internal = typeof INTERNAL;
