package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.TypeMismatch;

/**
 * No call throws: every public static method of the API, and every public method of a sample of each kind of object
 * it returns, called with null for each reference argument (and zero for each primitive), returns; a fallible one
 * returns a refusal, {@link TypeMismatch} for a null.
 */
public class NeverThrowsTest {

  private static Object zero(Class<?> t) {
    if (t == boolean.class) return false;
    if (t == long.class) return 0L;
    if (t == int.class) return 0;
    if (t == byte.class) return (byte) 0;
    return null;
  }

  private record Called(String method, Optional<Throwable> thrown, Object returned, boolean anyNull) {}

  private static Called call(Method m, Object target) {
    final Object[] args = Arrays.stream(m.getParameterTypes()).map(NeverThrowsTest::zero).toArray();
    final boolean anyNull = Arrays.stream(m.getParameterTypes()).anyMatch(t -> !t.isPrimitive());
    try {
      return new Called(m.toGenericString(), Optional.empty(), m.invoke(target, args), anyNull);
    } catch (InvocationTargetException e) {
      return new Called(m.toGenericString(), Optional.of(e.getCause()), null, anyNull);
    } catch (IllegalAccessException e) {
      return new Called(m.toGenericString(), Optional.of(e), null, anyNull);
    }
  }

  private static Stream<Method> methods(Class<?> c, boolean statics) {
    return Arrays.stream(c.getDeclaredMethods())
      .filter(m -> Modifier.isPublic(m.getModifiers()) && !m.isSynthetic() && Modifier.isStatic(m.getModifiers()) == statics)
      // the combinators run the caller's functions, which must not be null (Result's documentation)
      .filter(m -> !(c == Result.class && List.of("map", "flatMap", "mapErr", "fold").contains(m.getName())))
      // a result holds what it is given, null too (Result.ok(null) is the caller's)
      .filter(m -> !(c == Result.class && List.of("ok", "err").contains(m.getName())))
      // the platform's members of every enum (Enum.valueOf's contract), not this API's calls
      .filter(m -> !(c.isEnum() && m.getName().equals("valueOf")));
  }

  private static void check(List<Called> calls) {
    assertTrue(calls.size() > 0, "methods were called");
    assertEquals(List.of(), calls.stream().filter(c -> c.thrown().isPresent()).map(c -> c.method() + " threw " + c.thrown().get()).toList(), "calls that threw");
    final List<String> accepted = calls.stream()
      .filter(c -> c.anyNull() && c.returned() instanceof Result<?, ?> r && r.isOk())
      .map(Called::method)
      .toList();
    assertEquals(List.of(), accepted, "calls that accepted a null");
    final List<String> notMismatch = calls.stream()
      .filter(c -> c.anyNull() && c.returned() instanceof Result.Err<?, ?> e && !mismatch(e.error()))
      .map(c -> c.method() + " refused a null with " + c.returned())
      .toList();
    assertEquals(List.of(), notMismatch, "a null is a TypeMismatch");
  }

  private static boolean mismatch(Object error) {
    return error instanceof TypeMismatch
      || (error instanceof xyz.kd5ujc.dilf4s.error.RenderError.Canonical c && c.error() instanceof TypeMismatch);
  }

  @Test
  void staticMethodsWithNulls() throws Exception {
    final List<Called> calls = Api.publicClasses().stream().flatMap(c -> methods(c, true)).map(m -> call(m, null)).toList();
    assertTrue(calls.size() >= 120, calls.size() + " static methods");
    check(calls);
  }

  @Test
  void recordConstructorsWithNulls() throws Exception {
    final List<String> threw = Api.publicClasses().stream()
      .filter(Class::isRecord)
      .flatMap(c -> Arrays.stream(c.getDeclaredConstructors()).filter(k -> Modifier.isPublic(k.getModifiers())).flatMap(k -> {
        final Object[] args = Arrays.stream(k.getParameterTypes()).map(NeverThrowsTest::zero).toArray();
        try {
          k.newInstance(args);
          return Stream.<String>empty();
        } catch (InvocationTargetException e) {
          return Stream.of(c.getName() + " threw " + e.getCause());
        } catch (ReflectiveOperationException e) {
          return Stream.of(c.getName() + ": " + e);
        }
      }))
      .toList();
    assertEquals(List.of(), threw, "record constructors that threw on nulls");
  }

  /** A sample of every class of the API with instance methods of its own (records aside: their accessors take nothing). */
  private static List<Object> samples() throws java.io.IOException {
    final Value.Map map = V.ok(Value.map(java.util.Map.of("a", Value.integer(1))), "map");
    final Value.Map trie = V.file("trie_v1.json");
    final Value.Map fixture = V.rows(trie, "fixtures").get(0);
    final Trie.KeyMode mode = V.ok(Trie.fixed(2), "mode");
    final Value.Map versioned = V.file("versioned_trie_v1.json");
    final Bip39.WordList words = V.ok(Bip39.wordList(Bip39.Language.ENGLISH, java.util.stream.IntStream.range(0, 2048).mapToObj(i -> "w" + i).toList()), "words");
    final Jlvm.Program program = V.ok(Jlvm.parse("{\"+\": [1, 2]}"), "program");
    return List.of(
      Value.nul(), Value.bool(true), Value.integer(7), V.ok(Value.number(java.math.BigInteger.ONE, java.math.BigInteger.TWO), "1/2"),
      V.textValue("t"), V.bytesValue(new byte[] {1}), V.ok(Value.array(List.of(Value.nul())), "array"), map,
      V.ok(Hashing.digest(Hashing.Algorithm.SHA_256, new byte[0]), "digest"),
      V.ok(Hashing.domainTag("a/v1"), "tag"),
      mode,
      V.ok(Trie.check(Hashing.Algorithm.SHA_256, mode, V.hexAt(fixture, "root"), V.cbor(V.str(V.map(V.get(fixture, "proof")), "cbor"))), "a trie proof"),
      V.ok(VersionedTrie.verifyState((int) V.num(versioned, "width"), Hashing.Algorithm.SHA_256, V.hexAt(versioned, "root"),
        V.cbor(V.str(V.rows(versioned, "proofs").get(0), "cbor"))), "a state proof"),
      words,
      V.ok(Bip39.toMnemonic(new byte[16], words), "mnemonic"),
      program,
      V.ok(Jlvm.compile(program), "compiled"),
      Result.ok(1), Result.err("e")
    );
  }

  private static boolean ownInstanceMethods(Class<?> c) {
    return Arrays.stream(c.getDeclaredMethods())
      .anyMatch(m -> Modifier.isPublic(m.getModifiers()) && !Modifier.isStatic(m.getModifiers()) && !m.isSynthetic()
        && !List.of("equals", "hashCode", "toString").contains(m.getName()));
  }

  @Test
  void everyClassWithMethodsHasASample() throws Exception {
    final java.util.Set<Class<?>> sampled = samples().stream().map(Object::getClass).collect(java.util.stream.Collectors.toSet());
    final List<String> missing = Api.publicClasses().stream()
      .filter(c -> !c.isInterface() && !c.isRecord() && !c.isEnum() && ownInstanceMethods(c))
      .filter(c -> !sampled.contains(c))
      .map(Class::getName)
      .toList();
    assertEquals(List.of(), missing, "classes whose instance methods no sample calls");
  }

  @Test
  void instanceMethodsWithNulls() throws Exception {
    final List<Called> calls = samples().stream()
      .flatMap(s -> Stream.concat(methods(s.getClass(), false), Arrays.stream(s.getClass().getInterfaces()).flatMap(i -> methods(i, false)))
        .filter(m -> !List.of("equals", "hashCode", "toString").contains(m.getName()))
        .map(m -> call(m, s)))
      .toList();
    assertTrue(calls.size() >= 40, calls.size() + " instance methods");
    check(calls);
  }
}
