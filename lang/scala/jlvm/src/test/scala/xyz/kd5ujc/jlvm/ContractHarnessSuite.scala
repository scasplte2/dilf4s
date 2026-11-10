package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue

import weaver.SimpleIOSuite

object ContractHarnessSuite extends SimpleIOSuite {

  pureTest("contract rejection is identified by its case, never by diagnostic text") {
    import ContractHarness.StepError

    expect
      .same(true, ContractHarness.rejected(Left(StepError.Rejected(CanonicalValue.CNull))))
      .and(expect.same(false, ContractHarness.rejected(Left(StepError.Evaluation(JlvmError.Malformed("reject: forged"))))))
      .and(expect.same(false, ContractHarness.rejected(Left(StepError.MissingResult))))
      .and(expect.same(false, ContractHarness.rejected(Right(CanonicalValue.CNull))))
  }
}
