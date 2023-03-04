package exercises02

import scala.util.matching.Regex

object Counter {
  private final val wordPattern: Array[Char] = Array('(', ')', ' ', '.', ',', '!', '?', ':', '\n', '\t', '\r')
  private final val russianPattern: Regex    = "([A-z]+[-|'][A-z]+)|([A-z]+)".r
  private final val numberPattern: Regex     = "([0-9]+).([0-9]+)|([0-9]+),([0-9]+)|([0-9]+)".r

  private def wordPatternOut(regexp: Regex, text: String): Array[String] =
    for (word <- regexp.findAllIn(text).toArray) yield word.toLowerCase()

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    val acceptedWords: Array[String] = for (word <- text.toLowerCase.split(wordPattern) if word.nonEmpty) yield word
    acceptedWords.map(K => (K, acceptedWords.count(_ == K))).toMap
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    val acceptedRussianWords: Array[String] = wordPatternOut(russianPattern, text)
    acceptedRussianWords.map(K => (K, acceptedRussianWords.count(_ == K))).toMap
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    val accpetedNumbers: Array[String] = wordPatternOut(numberPattern, text)
    accpetedNumbers.map(K => (K, accpetedNumbers.count(_ == K))).toMap
  }
}
