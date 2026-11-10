package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue

import weaver.SimpleIOSuite

object ContractHarnessSuite extends SimpleIOSuite {

  pureTest("only StepError.Rejected is a contract rejection") {
    import ContractHarness.StepError

    expect
      .same(true, ContractHarness.rejected(Left(StepError.Rejected(CanonicalValue.CNull))))
      .and(
        expect.same(
          false,
          ContractHarness.rejected(
            Left(StepError.Evaluation(JlvmError.Malformed(JlvmError.MalformedReason.NotJson)))
          )
        )
      )
      .and(expect.same(false, ContractHarness.rejected(Left(StepError.MissingResult))))
      .and(expect.same(false, ContractHarness.rejected(Right(CanonicalValue.CNull))))
  }
}
