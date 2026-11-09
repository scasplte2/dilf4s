package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.KeyError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * BIP-39 mnemonics: entropy to words and back, with the checksum. The word list is the caller's: the reference reads
 * its bundled lists only in an effect ({@code WordList.load[F]}), so this layer takes the 2048 words (e.g. BIP-39's
 * {@code english.txt}) and checks them.
 *
 * <p>The 64-byte seed is not here: the reference's PBKDF2 ({@code Bip39.seed}) builds a lazy chain as deep as its 2048
 * rounds, and reading it overflows a small thread stack (448 KB does). The seed is PBKDF2-HMAC-SHA512 of the NFKD
 * phrase (its words joined by spaces) with the salt {@code "mnemonic"} + NFKD(passphrase), 2048 rounds, 64 bytes: JCA's
 * {@code PBKDF2WithHmacSHA512} computes it.
 */
public final class Bip39 {
  private Bip39() {}

  /** The BIP-39 languages, each with its phrase separator. */
  public enum Language {
    /** English. */
    ENGLISH,
    /** Japanese (separated by U+3000). */
    JAPANESE,
    /** Korean. */
    KOREAN,
    /** Spanish. */
    SPANISH,
    /** Simplified Chinese. */
    CHINESE_SIMPLIFIED,
    /** Traditional Chinese. */
    CHINESE_TRADITIONAL,
    /** French. */
    FRENCH,
    /** Italian. */
    ITALIAN,
    /** Czech. */
    CZECH,
    /** Portuguese. */
    PORTUGUESE
  }

  /** A checked word list: exactly 2048 distinct words. */
  public static final class WordList {
    private final xyz.kd5ujc.crypto.keys.WordList scala;
    private final Language language;

    private WordList(xyz.kd5ujc.crypto.keys.WordList scala, Language language) {
      this.scala = scala;
      this.language = language;
    }

    /**
     * The language.
     *
     * @return the language
     */
    public Language language() {
      return language;
    }
  }

  /**
   * {@code words} as {@code language}'s list, refused ({@code InvalidWordList}) unless they are 2048 distinct words.
   *
   * @param language the language
   * @param words the words, in order
   * @return the list, or the refusal
   */
  public static Result<WordList, KeyError> wordList(Language language, List<String> words) {
    return language == null || words == null || words.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("a language and words"))
      : Signatures.wordList(language, List.copyOf(words)).map(w -> new WordList(w, language));
  }

  /** A mnemonic phrase. {@code toString} never shows its words; equality compares them. */
  public static final class Mnemonic {
    private final xyz.kd5ujc.crypto.keys.Mnemonic scala;

    private Mnemonic(xyz.kd5ujc.crypto.keys.Mnemonic scala) {
      this.scala = scala;
    }

    /**
     * The words.
     *
     * @return the words, unmodifiable
     */
    public List<String> words() {
      return List.copyOf(Signatures.mnemonicWords(scala));
    }

    /**
     * The phrase: the words joined by the language's separator.
     *
     * @return the phrase
     */
    public String phrase() {
      return Signatures.mnemonicPhrase(scala);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Mnemonic m && Signatures.sameMnemonic(m.scala, scala);
    }

    @Override
    public int hashCode() {
      return Signatures.mnemonicWords(scala).size();
    }

    @Override
    public String toString() {
      return "Mnemonic(<" + Signatures.mnemonicWords(scala).size() + " words redacted>)";
    }
  }

  /**
   * A mnemonic and the entropy it encodes. The entropy is copied in and out and never shown.
   *
   * @param mnemonic the mnemonic
   * @param entropy the entropy
   */
  public record Decoded(Mnemonic mnemonic, byte[] entropy) {
    public Decoded {
      entropy = Bytes.copy(entropy);
    }

    @Override
    public byte[] entropy() {
      return Bytes.copy(entropy);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Decoded d && Objects.equals(d.mnemonic, mnemonic) && java.security.MessageDigest.isEqual(d.entropy, entropy);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(mnemonic);
    }

    @Override
    public String toString() {
      return "Decoded(" + mnemonic + ", <entropy redacted>)";
    }
  }

  /**
   * The mnemonic of 16, 20, 24, 28 or 32 bytes of entropy: 12 to 24 words.
   *
   * @param entropy the entropy
   * @param words the word list
   * @return the mnemonic, or {@code InvalidEntropy}
   */
  public static Result<Mnemonic, KeyError> toMnemonic(byte[] entropy, WordList words) {
    return entropy == null || words == null
      ? Result.err(Wrap.nullArgument("entropy and a word list"))
      : Signatures.toMnemonic(entropy, words.scala).map(Mnemonic::new);
  }

  /**
   * A phrase checked (word count, membership, checksum), and its entropy.
   *
   * @param phrase the phrase
   * @param words the word list
   * @return the mnemonic and its entropy, or {@code InvalidMnemonic}
   */
  public static Result<Decoded, KeyError> toEntropy(String phrase, WordList words) {
    return phrase == null || words == null
      ? Result.err(Wrap.nullArgument("a phrase and a word list"))
      : Signatures.toEntropy(phrase, words.scala).map(e -> new Decoded(new Mnemonic(e.getKey()), e.getValue()));
  }
}
