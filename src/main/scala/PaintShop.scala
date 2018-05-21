import PaintShop._

/**
  * PaintShop - class that processes seq of client colors and provides possible solution
  * @param colorsNum number of colors
  */

class PaintShop(colorsNum:Int) {

  var mixCases:Set[ColorList] = Set(ColorList.init(colorsNum))

  /**
    * Processing new Line of input.
    *
    * @param colors - Seq with tuples for color num and color type. Ex.: 1 M 3 G 5 G
    * @return true is new Mixes possible. False if no solutions exists
    */

  def processClientColors(colors:Seq[(Int, ColorType)]):Boolean = {
    val newMixCases = mixCases.flatMap({
      colorMix =>
        val newMixes = for(
          (colorNum, colorType) <- colors
        ) yield {
          val colorIndex = colorNum - 1 // Array colors starts from 0
          if(colorMix.mix(colorIndex).contains(colorType)) {
            if(colorMix.fixes(colorIndex)) {
              Some(colorMix.copy()) // color fixed in the prev iterations
            } else {
              Some(colorMix.withFixedColor(colorIndex)) // mix with new Fixed color
            }
          } else {
            if(colorMix.fixes(colorIndex)) {
              None // color fixed in the prev iterations
            } else {
              Some(colorMix.withUpdatedColorType(colorIndex, ColorTypes.matte)) // only one case when it's not fixed and different type - gloss. Changing to M and fixing
            }
          }
        }
        newMixes.flatten
    })
    mixCases = newMixCases

    newMixCases.nonEmpty
  }

  /**
    * Getting Option[ColorMix]  - a mix with a fewer mattes possible
    *
    * @return Option[ColorMix]
    */

  def getSolution:Option[ColorList] = {
    if(mixCases.nonEmpty) {
      val min = mixCases.map({
        mixCase =>
          val matteSum = mixCase.mix.foldLeft(0)((sum, optColorType) => if(optColorType.contains(ColorTypes.matte)) sum + 1 else sum)
          (matteSum, mixCase)
      }).minBy(_._1)._2
      Some(min)
    } else {
      None
    }
  }
}

object PaintShop {

  type Fixes = Array[Boolean]
  type Mix = Array[Option[ColorType]]
  type ColorType = Char
}

/**
  * Model for colors list
  *
  * @param mix - array of color types
  * @param fixes - array that shows if a color has been chosen by previous client in the list
  */

case class ColorList(mix:Mix, fixes:Fixes) {

  /**
    * Returning a copy of mix with fixed color
    * @param num - number of color to fix
    * @return ColorMix - new mix
    */
  def withFixedColor(num:Int):ColorList = {
    val newMix = ColorList(this.mix.clone(), this.fixes.clone())
    newMix.fixes(num) = true
    newMix
  }

  /**
    * Returning a copy of mix with updated and fixed color type
    * @param num - number of color to fix, color - new color type
    * @return ColorMix - new mix
    */
  def withUpdatedColorType(num:Int, colorType:ColorType):ColorList = {
    val newMix = ColorList(this.mix.clone(), this.fixes.clone())
    newMix.fixes(num) = true
    newMix.mix(num) = Some(colorType)
    newMix
  }
}

object ColorList {

  /**
    * Generates a ColorList object with defined size. All elements has G type
    * @param size
    * @return
    */

  def init(size:Int) = {
    new ColorList(
      Array.fill[Option[ColorType]](size)(Some(ColorTypes.gloss)),
      Array.fill[Boolean](size)(false)
    )
  }
}


object ColorTypes {
  val gloss:ColorType = 'G' //Glossy color type
  val matte:ColorType = 'M' //Matte color type
}
