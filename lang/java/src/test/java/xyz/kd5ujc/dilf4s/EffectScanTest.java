package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * The library's sources run one effect, in one place: {@code unsafeRunSync} appears once, in {@link Backends}'s {@code
 * run}, and nowhere in Scala; and the one {@code try}/{@code catch} is the one around it, catching {@code
 * LinkageError} alone.
 */
public class EffectScanTest {

  private static final Path SOURCES = Path.of(System.getProperty("dilf4s.sources"));

  private record Hit(Path file, int line, String text) {}

  private static List<Hit> find(String tree, Pattern pattern) throws IOException {
    try (Stream<Path> files = Files.walk(SOURCES.resolve(tree))) {
      return files.filter(Files::isRegularFile).flatMap(f -> {
        final List<String> lines = read(f);
        return java.util.stream.IntStream.range(0, lines.size())
          .filter(i -> pattern.matcher(lines.get(i)).find())
          .mapToObj(i -> new Hit(SOURCES.relativize(f), i + 1, lines.get(i).trim()));
      }).toList();
    }
  }

  private static List<String> read(Path f) {
    try {
      return Files.readAllLines(f, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new IllegalStateException(f.toString(), e);
    }
  }

  @Test
  void oneEffectInOnePlace() throws IOException {
    final List<Hit> java = find("main/java", Pattern.compile("unsafe"));
    final List<Hit> scala = find("main/scala", Pattern.compile("unsafe"));
    final List<Hit> tries = find("main/java", Pattern.compile("\\btry\\b"));
    final List<Hit> catches = find("main/java", Pattern.compile("\\bcatch\\s*\\("));
    final Matcher caught = catches.isEmpty() ? null : Pattern.compile("catch \\((\\w+) \\w+\\)").matcher(catches.get(0).text());
    assertAll(
      () -> assertEquals(List.of(), scala, "no unsafe call in the Scala sources"),
      () -> assertEquals(1, java.stream().filter(h -> h.text().contains("unsafeRunSync")).count(), "one unsafeRunSync in the Java sources: " + java),
      () -> assertEquals(List.of(Path.of("main/java/xyz/kd5ujc/dilf4s/Backends.java")), java.stream().filter(h -> h.text().contains("unsafeRunSync")).map(Hit::file).toList()),
      () -> assertTrue(java.stream().filter(h -> h.text().contains("unsafeRunSync")).allMatch(h -> h.text().startsWith("return load.unsafeRunSync()")), "in run: " + java),
      () -> assertEquals(List.of(Path.of("main/java/xyz/kd5ujc/dilf4s/Backends.java")), tries.stream().map(Hit::file).toList(), "one try, in Backends: " + tries),
      () -> assertEquals(1, catches.size(), "one catch: " + catches),
      () -> assertTrue(caught != null && caught.find() && caught.group(1).equals("LinkageError"), "it catches LinkageError alone: " + catches),
      () -> assertTrue(tries.get(0).line() < catches.get(0).line() && catches.get(0).line() - tries.get(0).line() <= 3, "around the one call: " + tries + catches)
    );
  }
}
