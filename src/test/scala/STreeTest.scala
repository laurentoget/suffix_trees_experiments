class STreeTest extends org.scalatest.FlatSpec {
  "Empty tree" should "get printed " in {
    println ("res:\n"+STree.empty(0,0).print(0))
  }

  "Single tree " should "get printed" in {
    println(STree.single("banana",0,0).print(0))
  }

  "Inserted tree" should "get printed " in {
    println(STree.single("abc",0,0).insert("ac",0,0).print(0))
  }

  "Suffix tree" should "get printed" in {
    println(STree.suffixTree("bananas",0).print(0))
  }

  "Compressed Suffix tree" should "get printed" in {
    println(STree.single("bananas",0,0).compressed().print(0))
    val s1 = STree.suffixTree("bananas",0)
    val s2 = s1.suffixTree("banalité",1)
    val s3 = s2.suffixTree("brutalité",2)

    println(s1.print(0))
    println(s2.print(0))
    println("s2:\n" + s2.compressed().print(0))
    println("s3:\n" + s3.compressed().print(0))

  }
}
