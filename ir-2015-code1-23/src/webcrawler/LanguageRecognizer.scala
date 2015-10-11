package webcrawler

import scala.collection.mutable.{Map => MutMap}

/**
 * @author Ivaylo
 */
object LanguageRecognizer {
  // Variable for the precomputed ngrams for English and German
  val tokens: Map[String, Map[String,Double]] = getTokens

  /**
   * A function that recognizes the language of a given String.
   *
   * @param s The string whose language should be recognized.
   * @return "en" for English and "de" for German.
   */
	def recognize(s:String): String = {
		val probs = MutMap[String, Double]()
		for( (key, value) <- tokens ) {
			val nGrams = s.sliding(4)
			var result = 0.0
			val minV = value("minimum")
			nGrams.foreach( ngram => result += value.getOrElse(ngram, minV) )
			probs(key) = result
		}
		probs.reduceLeft( gProb )._1
	}

  /**
   * A helper function that compares a language-probability pairs
   * and returns the more probable pair.
   *
   * @param s1 The first language-probability pair to be compared.
   * @param s2 The second language-probability pair to be compared.
   * @return The language-probability pair with higher probability.
   */
	def gProb(s1: (String, Double), s2: (String, Double)): (String, Double) = {
		if(s1._2 > s2._2) s1 else s2
	}

  /**
   * Loads the ngrams and their frequencies for English and German.
   *
   * @return Map consisting of language - Map[ngram, frequency].
   *  This means that for both English and German maps that
   *  contain all of the ngrams and their frequencies are generated.
   */
	def getTokens: Map[String, Map[String,Double]] = {
		val langs = List("en", "de")
		val tokensMap = MutMap[String, Map[String,Double]]()
		for(l <- langs) {
			val fileLines = io.Source.fromFile("ngrams/counts."+l).getLines()
			val current = MutMap[String,Double]()
			for(line <- fileLines) {
				val keyValue = line.trim.split("\\t+")
				current(keyValue(0)) = keyValue(1).toDouble
			}
			tokensMap(l) = current.toMap
		}
		tokensMap.toMap
	}
}
