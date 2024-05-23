import org.scalacheck.Gen

package object generators {
  // Key-Value pairs generator
  val kvGen: Gen[(Int, String)] = for {
    key   <- Gen.posNum[Int]
    value <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield (key, value)

  // Generate a list of unique Key-Value pairs
  def kvListGenUniqueKeys(n: Int): Gen[List[(Int, String)]] =
    Gen.listOfN(n, kvGen).map(list => list.groupBy(_._1).values.map(_.head).toList).suchThat(_.nonEmpty)

}
