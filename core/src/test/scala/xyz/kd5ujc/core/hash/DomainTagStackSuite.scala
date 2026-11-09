package xyz.kd5ujc.core.hash

import xyz.kd5ujc.test.stack.StackProbe

import weaver.SimpleIOSuite

/** A domain tag of any number of segments is read at a normal stack (the regular expression recursed once per segment). */
object DomainTagStackSuite extends SimpleIOSuite {

  private val segments = 200000
  private val many = "dilf4s/" + "seg/" * segments

  test(s"a tag of $segments segments is accepted, and one without its version refused") {
    StackProbe.fits("the tags")((DomainTag(many + "v1").isRight, DomainTag(many + "v").isLeft)) {
      case (accepted, refused) =>
        expect(accepted, "the well-formed tag is accepted").and(expect(refused, "the tag without a version is refused"))
    }
  }
}
