package xyz.kd5ujc.core.hash

import cats.syntax.all._

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/** The tag scanner accepts exactly the language of the regular expression it replaced. */
object DomainTagSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = super.checkConfig.withMinimumSuccessful(5000)

  // the reference: the grammar as java.util.regex read it, whole (`matches`)
  private val Reference = "^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$".r.pattern

  private def agrees(s: String) = expect.same(s -> Reference.matcher(s).matches(), s -> DomainTag(s).isRight)

  // every class the grammar names, its separators, and characters outside it: upper case, a line end, non-ASCII
  private val char = Gen.oneOf("az09v/._-A\néK".toVector)
  private val part = Gen.oneOf("", "a", "z9", "v", "v0", "v12", "V1", ".a", "a.", "a-b_c.d", "-", "_x", "a\n", "é", "0v1")

  test("on text over the grammar's alphabet") {
    forall(Gen.choose(0, 16).flatMap(n => Gen.listOfN(n, char)).map(_.mkString))(agrees)
  }

  test("on '/'-joined parts, valid and nearly so") {
    forall(Gen.choose(1, 6).flatMap(n => Gen.listOfN(n, part)).map(_.mkString("/")))(agrees)
  }

  pureTest("on the edges") {
    List(
      "",
      "v1",
      "/v1",
      "a/v1",
      "a/v",
      "a/v1/",
      "a//v1",
      "a/b/v1",
      "a/.b/v1",
      "a/b./v1",
      "a/b-c_d.e/v10",
      "A/v1",
      "a/V1",
      "a/v1\n",
      "a/v1\r\n",
      "a\n/v1",
      "a/v1a",
      "a/b/v1/v2",
      "0/9/v0",
      "a/v01",
      "a/vv1",
      "ab",
      "a/b",
      "dilf4s/mpt/leaf/v1",
      "a/é/v1",
      "a/v١"
    ).foldMap(agrees)
  }
}
