package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * The public API names Java types and its own, nothing else: every type in a public class's signatures (supertypes,
 * permitted subclasses, record components, fields, constructors, methods, type parameters, generic arguments) is
 * {@code java.*}, a primitive, or a public type of {@code xyz.kd5ujc.dilf4s} outside {@code internal}. So no Scala,
 * scodec, cats or reference type reaches a caller.
 */
public class ApiShapeTest {

  private static boolean allowed(Class<?> c) {
    final Class<?> base = c.isArray() ? c.getComponentType() : c;
    if (base.isArray()) return allowed(base);
    if (base.isPrimitive()) return true;
    final String name = base.getName();
    return name.startsWith("java.") || (name.startsWith("xyz.kd5ujc.dilf4s.") && !name.startsWith("xyz.kd5ujc.dilf4s.internal.") && Api.visible(base));
  }

  private static Stream<Class<?>> classesIn(Type t) {
    return switch (t) {
      case Class<?> c -> Stream.of(c);
      case ParameterizedType p -> Stream.concat(
        Stream.concat(classesIn(p.getRawType()), Optional.ofNullable(p.getOwnerType()).stream().flatMap(ApiShapeTest::classesIn)),
        Arrays.stream(p.getActualTypeArguments()).flatMap(ApiShapeTest::classesIn));
      case WildcardType w -> Stream.concat(Arrays.stream(w.getUpperBounds()), Arrays.stream(w.getLowerBounds())).flatMap(ApiShapeTest::classesIn);
      case TypeVariable<?> v -> Arrays.stream(v.getBounds()).flatMap(ApiShapeTest::classesIn);
      case GenericArrayType a -> classesIn(a.getGenericComponentType());
      default -> Stream.empty();
    };
  }

  private static boolean exposed(int modifiers) {
    return Modifier.isPublic(modifiers) || Modifier.isProtected(modifiers);
  }

  private static Stream<String> signatures(Class<?> c) {
    final Stream<Type> supers = Stream.concat(Optional.ofNullable(c.getGenericSuperclass()).stream(), Arrays.stream(c.getGenericInterfaces()));
    final Stream<Type> permitted = Optional.ofNullable(c.getPermittedSubclasses()).stream().flatMap(Arrays::stream);
    final Stream<Type> components = Optional.ofNullable(c.getRecordComponents()).stream().flatMap(Arrays::stream).map(RecordComponent::getGenericType);
    final Stream<Type> fields = Arrays.stream(c.getDeclaredFields()).filter(f -> exposed(f.getModifiers())).map(Field::getGenericType);
    final Stream<Type> constructors = Arrays.stream(c.getDeclaredConstructors())
      .filter(k -> exposed(k.getModifiers()))
      .flatMap(k -> Stream.concat(Arrays.stream(k.getGenericParameterTypes()), Arrays.stream(k.getTypeParameters())));
    final Stream<Type> methods = Arrays.stream(c.getDeclaredMethods())
      .filter(m -> exposed(m.getModifiers()) && !m.isSynthetic())
      .flatMap(m -> Stream.of(
        Stream.of(m.getGenericReturnType()),
        Arrays.stream(m.getGenericParameterTypes()),
        Arrays.stream(m.getGenericExceptionTypes()),
        Arrays.stream(m.getTypeParameters())
      ).flatMap(s -> s));
    final Stream<Type> types = Stream.of(supers, permitted, components, fields, constructors, methods, Arrays.stream(c.getTypeParameters()).map(t -> (Type) t))
      .flatMap(s -> s);
    return types.flatMap(ApiShapeTest::classesIn).filter(k -> !allowed(k)).map(k -> c.getName() + " names " + k.getName()).distinct();
  }

  @Test
  void noForeignTypeInThePublicApi() throws Exception {
    final List<Class<?>> classes = Api.publicClasses();
    assertTrue(classes.size() >= 150, classes.size() + " public classes found");
    final List<String> leaks = classes.stream().flatMap(ApiShapeTest::signatures).toList();
    assertEquals(List.of(), leaks, "types outside the API in its public signatures");
  }

  @Test
  void theScanSeesAForeignType() throws Exception {
    // the scan itself: an internal class names the reference's types, so scanning it must report them
    final List<String> internal = signatures(xyz.kd5ujc.dilf4s.internal.Values.class).toList();
    assertTrue(internal.stream().anyMatch(s -> s.contains("scala.")), "the scan reports a Scala type: " + internal);
    assertTrue(internal.stream().anyMatch(s -> s.contains("xyz.kd5ujc.core.")), "the scan reports a reference type: " + internal);
  }

  @Test
  void everyFamilyIsSealedAndEveryChoiceRecordOrFinal() throws Exception {
    final List<String> open = Api.publicClasses().stream()
      .filter(c -> c.isInterface() && !c.isAnnotation())
      .filter(c -> !c.isSealed())
      .map(Class::getName)
      .toList();
    assertEquals(List.of(), open, "public interfaces a caller could implement");
    final List<String> extendable = Api.publicClasses().stream()
      .filter(c -> !c.isInterface() && !c.isEnum() && !c.isRecord() && !Modifier.isFinal(c.getModifiers()))
      .map(Class::getName)
      .toList();
    assertEquals(List.of(), extendable, "public classes a caller could extend");
  }
}
