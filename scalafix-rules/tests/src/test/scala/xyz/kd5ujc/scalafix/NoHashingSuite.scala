package xyz.kd5ujc.scalafix

import org.scalatest.funsuite.AnyFunSuiteLike
import scalafix.testkit.AbstractSemanticRuleSuite

/** Runs NoHashing over every fixture in scalafix-rules/input: each marked line must be refused, and nothing else. */
class NoHashingSuite extends AbstractSemanticRuleSuite with AnyFunSuiteLike {
  runAllTests()
}
