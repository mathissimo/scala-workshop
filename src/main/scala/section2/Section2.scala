package section2

object Section2 {

  // Arrays

  def loud(input: Array[String]): Array[String] = {
    input.map(_.toUpperCase)
  }

  def concatenate(input: Array[String]): String = {
    var buffer = ""
    for (element <- input) {
      buffer += element + " "
    }
    buffer.trim
  }

  // Seq and List

  def loud(input: Seq[String]): Seq[String] = {
    input.map(_.toUpperCase)
  }

  def extractWords(input: Seq[String]): Seq[String] = {
    for (
      element <-input;
      subelement <- element.split(" ").toSeq
    ) yield subelement
  }

  // Map

  def loud(input: Map[String, String]): Map[String, String] = {
    input.map(element => element._1.toUpperCase -> element._2.toUpperCase)
  }

  def counts(input: Seq[String]): Map[String, Int] = {
    extractWords(input).foldLeft(Map.empty[String, Int])((countMap, word)=>{countMap + (word -> (countMap.getOrElse(word,0)+1))})
  }

  def counts1(input: Seq[String]): Map[String, Int] = {
    extractWords(input).groupBy(identity).mapValues(word => word.length)
  }


  def counts2(input: Seq[String]): Map[String, Int] = {
    var wordSet: Map[String, Int] = Map()
    extractWords(input).foreach(word => {
      val found = wordSet.getOrElse(word, 0)
      wordSet += (word->(found+1))
    })
    wordSet
  }

  def addTryFor(c: Char): Unit = ???
  def howManyTries(c: Char): Int = ???

}
