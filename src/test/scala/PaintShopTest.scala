import org.specs2._

class PaintShopTest extends mutable.Specification {

  "PaintShop should" >> {
    "generate a mix case for '1 G'" >> {
      val paintShop = new PaintShop(1)

      paintShop.processClientColors(Seq((1, 'G')))

      paintShop.mixCases.size mustEqual 1
      paintShop.mixCases.head.mix.length mustEqual 1
      paintShop.mixCases.head.fixes.length mustEqual 1
      paintShop.mixCases.head.mix.head mustEqual Some(ColorTypes.gloss)
      paintShop.mixCases.head.fixes.head must beTrue
    }

    "generate a mix case for '1 M'" >> {
      val paintShop = new PaintShop(1)

      paintShop.processClientColors(Seq((1, 'M')))

      paintShop.mixCases.size mustEqual 1
      paintShop.mixCases.head.mix.length mustEqual 1
      paintShop.mixCases.head.fixes.length mustEqual 1
      paintShop.mixCases.head.mix.head mustEqual Some(ColorTypes.matte)
      paintShop.mixCases.head.fixes.head must beTrue
    }

    "generate a solution for '5\n1 M 3 G 5 G\n2 G 3 M 4 G\n5 M'" >> {
      val paintShop = new PaintShop(5)

      paintShop.processClientColors(Seq((1, 'M'), (3, 'G'), (5, 'G')))
      paintShop.processClientColors(Seq((2, 'G'), (3, 'M'), (4, 'G')))
      paintShop.processClientColors(Seq((5, 'M')))


      paintShop.getSolution.isDefined must beTrue
      paintShop.getSolution.get.mix.flatten.mkString(" ") mustEqual "G G G G M"
    }

    "generate a solution for '2\n1, 'G')2 M\n1 M'" >> {
      val paintShop = new PaintShop(2)

      paintShop.processClientColors(Seq((1, 'G'), (2, 'M')))
      paintShop.processClientColors(Seq((1, 'M')))


      paintShop.getSolution.isDefined must beTrue
      paintShop.getSolution.get.mix.flatten.mkString(" ") mustEqual "M M"
    }

    "generate no solution for '1\n1 G\n1 M'" >> {
      val paintShop = new PaintShop(2)

      paintShop.processClientColors(Seq((1, 'G')))
      paintShop.processClientColors(Seq((1, 'M')))

      paintShop.getSolution.isDefined must beFalse
    }

    "generate a solution for '5\n1 M 3 G 5 G\n2 G 3 M 4 G\n5 M'" >> {
      val paintShop = new PaintShop(5)

      paintShop.processClientColors(Seq((1, 'M'), (3, 'G'), (5, 'G')))
      paintShop.processClientColors(Seq((2, 'G'), (3, 'M'), (4, 'G')))
      paintShop.processClientColors(Seq((5, 'M')))

      paintShop.getSolution.isDefined must beTrue
      paintShop.getSolution.get.mix.flatten.mkString(" ") mustEqual "G G G G M"
    }


    "generate a solution for '5\n2 M\n5 G\n1 G\n5 G 1 G 4 M\n3 G\n5 G\n3 G 5 G 1 G\n3 G\n2 M\n5 G 1 G\n2 M\n5 G\n4 M\n5 G 4 M\n'" >> {
      val paintShop = new PaintShop(5)

      paintShop.processClientColors(Seq((2, 'M')))
      paintShop.processClientColors(Seq((5, 'G')))
      paintShop.processClientColors(Seq((1, 'G')))

      paintShop.processClientColors(Seq((5, 'G'), (1, 'G'), (4, 'M')))
      paintShop.processClientColors(Seq((3, 'G')))
      paintShop.processClientColors(Seq((5, 'G')))

      paintShop.processClientColors(Seq((3, 'G'), (5, 'G'), (1, 'G')))

      paintShop.processClientColors(Seq((3, 'G')))
      paintShop.processClientColors(Seq((2, 'M')))

      paintShop.processClientColors(Seq((5, 'G'),(1, 'G')))

      paintShop.processClientColors(Seq((2, 'M')))
      paintShop.processClientColors(Seq((5, 'G')))
      paintShop.processClientColors(Seq((4, 'M')))

      paintShop.processClientColors(Seq((5, 'G'), (4, 'M')))


      paintShop.getSolution.isDefined must beTrue
      paintShop.getSolution.get.mix.flatten.mkString(" ") mustEqual "G M G M G"
    }

  }

}
