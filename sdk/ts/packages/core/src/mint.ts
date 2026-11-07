/**
 * Private brands that a class stamps onto an instance only when its constructor is given the class's module-private
 * token (internal to this SDK). A constructor is private to TypeScript alone: JavaScript can still call one (`new`,
 * `Reflect.construct`), and every instance it builds carries the class's own `#private` fields. A brand from
 * {@link mint} is stamped only by a call that held the token, so what a caller builds by calling a constructor from
 * outside fails the check, as an object borrowing the prototype, or a Proxy of an instance, does.
 */

/** Its constructor returns the object it is given, so a subclass's private fields are installed on that object. */
// eslint-disable-next-line @typescript-eslint/no-extraneous-class -- the constructor's return is the point: see mint
class Identity {
  constructor(o: object) {
    return o;
  }
}

/** A brand: `stamp` installs it on an object (before the object is frozen), `has` checks it in O(1). */
export interface Mint {
  /** Installs the brand on `o`, which must not be frozen yet and must not carry it already. */
  readonly stamp: (o: object) => void;
  /** Whether `x` carries the brand: one private-brand check, so no Proxy trap and no caller code runs. */
  readonly has: (x: unknown) => boolean;
}

/** A fresh brand, distinct from every other. */
export function mint(): Mint {
  let marked!: (x: object) => boolean;
  class Mark extends Identity {
    readonly #mark = true;
    static {
      marked = (x) => #mark in x;
    }
  }
  return Object.freeze({
    stamp: (o: object): void => {
      new Mark(o);
    },
    has: (x: unknown): boolean => ((typeof x === "object" && x !== null) || typeof x === "function") && marked(x),
  });
}
