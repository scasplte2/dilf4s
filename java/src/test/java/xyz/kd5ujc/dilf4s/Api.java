package xyz.kd5ujc.dilf4s;

import java.io.IOException;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/** The public API's classes, found on disk next to {@link Value}'s: public reflection only. */
final class Api {
  private Api() {}

  static List<Class<?>> publicClasses() throws IOException, URISyntaxException {
    final Path root = Path.of(Value.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    final Path base = root.resolve("xyz/kd5ujc/dilf4s");
    try (Stream<Path> files = Files.walk(base)) {
      return files
        .filter(p -> p.toString().endsWith(".class"))
        .map(p -> root.relativize(p).toString().replace('/', '.').replaceAll("\\.class$", ""))
        .filter(n -> !n.startsWith("xyz.kd5ujc.dilf4s.internal."))
        .map(Api::load)
        .filter(Api::visible)
        .sorted(Comparator.comparing(Class::getName))
        .toList();
    }
  }

  private static Class<?> load(String name) {
    try {
      return Class.forName(name, false, Api.class.getClassLoader());
    } catch (ClassNotFoundException e) {
      throw new IllegalStateException(name, e);
    }
  }

  /** Public, and every class it is nested in public: reachable by a caller. */
  static boolean visible(Class<?> c) {
    return Modifier.isPublic(c.getModifiers()) && !c.isAnonymousClass() && !c.isSynthetic()
      && (c.getEnclosingClass() == null || visible(c.getEnclosingClass()));
  }
}
