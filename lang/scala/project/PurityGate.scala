import wartremover.Wart

object PurityGate {

  val warts: Seq[Wart] = Seq(
    Wart.Var,
    Wart.While,
    Wart.Throw,
    Wart.Return,
    Wart.Null,
    Wart.MutableDataStructures,
    Wart.TryPartial,
    Wart.OptionPartial,
    Wart.EitherProjectionPartial,
    Wart.IterableOps,
    Wart.AsInstanceOf,
    Wart.IsInstanceOf
  )
}
