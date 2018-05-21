import java.util.Scanner

import PaintShop._


object Main {

  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    val colorsSize = scanner.nextLine().toInt
    val paintShop = new PaintShop(colorsSize)

    var line = scanner.nextLine()
    var solutionsExists = true
    while(line != "" && solutionsExists) {
      val values = line.split(" ")
      val colors = for(
        i <- 0 until values.size by 2
      ) yield {
        val colorNum = values(i).toInt
        val colorType: ColorType = values(i + 1).toCharArray()(0)
        (colorNum, colorType)
      }
      solutionsExists = paintShop.processClientColors(colors)
      line = scanner.nextLine()
    }
    if(solutionsExists) {
      paintShop.getSolution.foreach(solution => println(solution.mix.flatten.mkString(" ")))
    } else {
      println("No solution exists")
    }
  }
}


