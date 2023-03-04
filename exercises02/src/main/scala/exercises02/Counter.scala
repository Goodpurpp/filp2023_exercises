package exercises02

import scala.util.matching.Regex

object Counter {
  private final val splitterRegex         = "[\\s.,!?:\n\t\r()]+"
  private final val englishPattern: Regex = "([A-z]+[-|'][A-z]+)|([A-z]+)".r
  private final val numberPattern: Regex  = "([0-9]+).([0-9]+)|([0-9]+),([0-9]+)|([0-9]+)".r

  private def wordPatternOut(regexp: Regex, text: String): Map[String, Int] =
    regexp.findAllIn(text).toArray.map(word => word).groupMapReduce(_.toLowerCase)(_ => 1)(_ + _)

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    text
      .split(splitterRegex)
      .withFilter(word => word.nonEmpty)
      .map(word => word)
      .groupMapReduce(_.toLowerCase)(_ => 1)(_ + _)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = wordPatternOut(englishPattern, text)

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = wordPatternOut(numberPattern, text)
}
