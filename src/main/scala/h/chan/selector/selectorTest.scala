package h.chan.selector

// import org.openjdk.jmh.annotations._

// @State(Scope.Benchmark)
object SelectorTest {

	// @Benchmark
  def main(args: Array[String]): Unit = {
    test()
    val loop = 1000
    var i = loop
    var start = System.nanoTime
    while (i > 0) {
      bench()
      i -= 1
    }
    var now = System.nanoTime
    var benchmark = (now -start)/1e6
    println(s"warm | $loop loops | time: $benchmark ms")

    i = loop
    start = System.nanoTime
    while (i > 0) {
      bench()
      i -= 1
    }
    now = System.nanoTime
    benchmark = (now -start)/1e6
    println(s"bench | $loop loops | time: $benchmark ms")
  }

  def test() = {
    assert(
      Selector("a").contains("a")
    )
    assert(
      !Selector("a").contains("b")
    )
    assert(
      !Selector("div a").contains("a")
    )

    assert(
      Selector(":nth-child(6)").contains(":nth-child(6-6n)")
    )

    assert(
      Selector(":nth-last-child(6)").contains(":nth-last-child(6-6n)")
    )

    assert(
      !Selector(":nth-child(2n+3)").contains(":nth-child(2n-1)")
    )

    assert(
      !Selector("[title*=hello]").contains("[title]")
    )

    assert(
      Selector("*").contains("a, b")
    )

    assert(
      !Selector("a").contains("*")
    )
    assert(
      Selector(".x").contains(".x")
    )
    assert(
      Selector(".x").contains("a.x")
    )
    assert(
      Selector(".x").contains("*.x")
    )
    assert(
      Selector(".x").contains(".x.y")
    )
    assert(
      Selector(".x").contains(".y.x")
    )
    assert(
      !Selector(".x").contains(".y")
    )
    assert(
      !Selector(".x.y").contains(".y")
    )
    assert(
      !Selector("a.x").contains(".x")
    )
    // #1 is not a valid selector
    assert(
      Selector("a#a1").contains("a#a1")
    )
    assert(
      Selector("a#a1").contains("a.x.y#a1")
    )
    assert(
      Selector("a.x#a1").contains("a#a1.x")
    )
    assert(
      !Selector("a.x#a1.y").contains("a#a1")
    )
    assert(
      Selector("[title]").contains("[title]")
    )
    assert(
      Selector("[title]").contains("a[title=hello]")
    )
    assert(
      Selector("[title^=h]").contains("a[title=hello]")
    )
    assert(
      Selector("[title$=o]").contains("a[title=hello]")
    )
    assert(
      Selector("[lang|=en]").contains("a[lang=en-US]")
    )
    assert(
      !Selector("[lang|=zh]").contains("a[lang=zht]")
    )
    assert(
      Selector(":nth-child(1)").contains(":nth-child(1)")
    )
    assert(
      Selector(":first-child").contains(":nth-child(1)")
    )
    assert(
      Selector(":nth-child(even)").contains(":nth-child(2n)")
    )

    assert(
      Selector(":nth-child(odd)").contains(":nth-child(2n+1)")
    )

    assert(
      Selector(":first-of-type").contains(":first-child")
    )

    assert(
      Selector(":nth-child(odd)").contains(":first-child")
    )

    assert(
      !Selector(":nth-child(even)").contains(":first-child")
    )

    assert(
      Selector(":nth-child(even)").contains(":nth-child(4n)")
    )

    assert(
      Selector(":nth-child(n)").contains(":nth-child(3n-1)")
    )

    assert(
      Selector(":nth-child(n)").contains(":nth-child(-3n+1)")
    )

    assert(
      !Selector(":nth-child(6-n)").contains(":nth-child(n)")
    )
    assert(
      Selector(":nth-child(n)").contains(":nth-child(8-n)")
    )
    assert(
      Selector(":nth-child(8-n)").contains(":nth-child(8-2n)")
    )
    assert(
      Selector(":nth-child(6-n)").contains("a:nth-child(6-n)")
    )

    assert(
      Selector("div>a").contains("div>a")
    )

    assert(
      !Selector("div a").contains("div+a")
    )
    assert(
      !Selector("div>a").contains("section>a")
    )

    assert(
      Selector("div a").contains("div>a")
    )

    assert(
      Selector("div * a").contains("div>div>a")
    )
    assert(
      Selector("section a").contains("section>div>a")
    )
    assert(
      Selector("body>section a").contains("body>section>div>a")
    )
    assert(
      Selector("*").contains("div#a1>a:first-of-type>span:nth-of-type(3)")
    )
    assert(
      Selector("div#a1 span").contains("div#a2>div#a1>a:first-of-type>span:nth-of-type(3)")
    )
    assert(
      !Selector("div#a1 span:first-child").contains("div#a1>a:first-of-type>span:nth-of-type(3)")
    )

    assert(
      Selector("div#a1 span:nth-of-type(n)").contains("div#a1>a:first-of-type>span:nth-of-type(3)")
    )

    assert(
     Selector("a:first-of-type").contains("span#a1 + a:first-child")
    )

    assert(
      Selector("div a:first-of-type").contains("div>span#a1+a:first-of-type")
    )

    println("All test passed")
  }

  def bench() = {
      Selector("a").contains("a")
      !Selector("a").contains("b")
      !Selector("div a").contains("a")
      Selector(":nth-child(6)").contains(":nth-child(6-6n)")
      Selector(":nth-last-child(6)").contains(":nth-last-child(6-6n)")


      !Selector(":nth-child(2n+3)").contains(":nth-child(2n-1)")



      !Selector("[title*=hello]").contains("[title]")



      Selector("*").contains("a, b")



      !Selector("a").contains("*")


      Selector(".x").contains(".x")


      Selector(".x").contains("a.x")


      Selector(".x").contains("*.x")


      Selector(".x").contains(".x.y")


      Selector(".x").contains(".y.x")


      !Selector(".x").contains(".y")


      !Selector(".x.y").contains(".y")


      !Selector("a.x").contains(".x")

    // #1 is not a valid selector

      Selector("a#a1").contains("a#a1")


      Selector("a#a1").contains("a.x.y#a1")


      Selector("a.x#a1").contains("a#a1.x")


      !Selector("a.x#a1.y").contains("a#a1")


      Selector("[title]").contains("[title]")


      Selector("[title]").contains("a[title=hello]")


      Selector("[title^=h]").contains("a[title=hello]")


      Selector("[title$=o]").contains("a[title=hello]")


      Selector("[lang|=en]").contains("a[lang=en-US]")


      !Selector("[lang|=zh]").contains("a[lang=zht]")


      Selector(":nth-child(1)").contains(":nth-child(1)")


      Selector(":first-child").contains(":nth-child(1)")


      Selector(":nth-child(even)").contains(":nth-child(2n)")



      Selector(":nth-child(odd)").contains(":nth-child(2n+1)")



      Selector(":first-of-type").contains(":first-child")



      Selector(":nth-child(odd)").contains(":first-child")



      !Selector(":nth-child(even)").contains(":first-child")



      Selector(":nth-child(even)").contains(":nth-child(4n)")



      Selector(":nth-child(n)").contains(":nth-child(3n-1)")



      Selector(":nth-child(n)").contains(":nth-child(-3n+1)")



      !Selector(":nth-child(6-n)").contains(":nth-child(n)")


      Selector(":nth-child(n)").contains(":nth-child(8-n)")


      Selector(":nth-child(8-n)").contains(":nth-child(8-2n)")


      Selector(":nth-child(6-n)").contains("a:nth-child(6-n)")



      Selector("div>a").contains("div>a")


      !Selector("div>a").contains("section>a")


      Selector("div a").contains("div>a")


      Selector("div * a").contains("div>div>a")


      Selector("section a").contains("section>div>a")


      Selector("body>section a").contains("body>section>div>a")


      Selector("*").contains("div#a1>a:first-of-type>span:nth-of-type(3)")


      Selector("div#a1 span").contains("div#a2>div#a1>a:first-of-type>span:nth-of-type(3)")


      !Selector("div#a1 span:first-child").contains("div#a1>a:first-of-type>span:nth-of-type(3)")



      Selector("div#a1 span:nth-of-type(n)").contains("div#a1>a:first-of-type>span:nth-of-type(3)")



     Selector("a:first-of-type").contains("span#a1 + a:first-child")



      Selector("div a:first-of-type").contains("div>span#a1+a:first-of-type")


  }

}
