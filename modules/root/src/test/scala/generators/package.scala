import org.scalacheck.Gen

package object generators {
  // Key-Value pairs generator
  val kvGen: Gen[(Int, String)] = for {
    key <- Gen.posNum[Int]
    value <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield (key, value)

}
