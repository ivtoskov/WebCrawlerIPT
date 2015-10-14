package webcrawler

import scala.collection.mutable.{Map => MutMap, MutableList}

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.util.control.Breaks._
import scala.util.hashing.MurmurHash3

class WebCrawlerManager(val baseUrl: String) {
  // A map that contains all of the links on the frontier
  var linkMap : MutMap[String,Boolean] = null
  // A variable that denotes the domain limit which should not be leaved
  var domainLimit = ""
  // A term to be matched. In this case 'student'
  private val term = "(?<=^|\\s)student(?=\\s|$)".r
  // A counter variable for the term 'student'
  private var studentCount = 0
  // A counter for the number of pages in English
  private var englishCount = 0
  // A counter for valid links
  private var validLinksCount = 0
  // A List containing the fingerprint of each non-duplicate document
  private var fingerprints: MutableList[String] = null
  // A counter for exact Duplicates
  private var exact_dup_counter = 0
  //A counter for near duplicates
  private var near_dup_counter = 0

  /**
   * This is the entry point of the Crawler. The
   * method takes care of the necessary initialization
   * and starts the actual crawling.
   */
  def run() {
    val domain = LinkHelper.splitURL(baseUrl)
    domainLimit = domain.substring(0, domain.lastIndexOf("www"))
    //println("base domain: " + domain)
    //println("base domainLimit: " + domainLimit)

    linkMap = MutMap[String, Boolean]()
    linkMap += baseUrl -> false

    fingerprints = MutableList[String]()

    time {this.crawlEntry()}
  }

  /**
   * The method where the actual crawling happens.
   * The method iterates through all the unvisited
   * links and crawls through their content in the
   * method crawl(url : String, domain: String).
   */
  private def crawlEntry() {
    var nextLink = linkMap.find(!_._2).orNull
    while(nextLink != null) {
      crawl(nextLink._1)
      nextLink = linkMap.find(!_._2).orNull
    }

    println("Distinct URLs found: " + linkMap.size)
    println("Exact duplicates found: " + exact_dup_counter)
    println("Visited pages: " + validLinksCount)
    println("Unique English pages found: " + englishCount)
    println("Near Duplicates found: " + near_dup_counter)
    println("Term frequency of \"student\": " + studentCount)
  }

  /**
   * This method visits a single URL and processes its contents.
   *
   * @param url The base url to be visited and crawled
   */
  private def crawl(url : String) {
    linkMap(url) = true
    // println("Current link: " + url)
    val pageContents = LinkHelper.readUrlContent(url)
    val domain = url.substring(0, url.lastIndexOf("/") + 1)
    if(!(pageContents == null)) {
      validLinksCount += 1
      LinkHelper.processLinks(LinkHelper.getLinks(pageContents), domain, linkMap, domainLimit)
      val doc: Document = Jsoup.parse(pageContents)
      val section = doc.getElementsByTag("section")
      section.select("nav").remove()
      section.select("aside").remove()


      val relevantContent = doc.body().text()

      // Exact and near duplicates detection
      val nonDuplicate = DuplicateFinder.analyse(relevantContent)

      if(nonDuplicate)
      {
        studentCount += term.findAllMatchIn(relevantContent.toLowerCase).length
        if(LanguageRecognizer.recognize(relevantContent) == "en")
          englishCount += 1
      }
      // println("Number of links: " + linkMap.size)
    }

  }

  /**
   * A method that measures the time needed for the execution
   * of a particular task.
   * @param block The task whose execution time should be measured
   * @tparam R Template parameter
   * @return The result of the computation
   */
  private def time[R](block: => R): R = {
    val t0 = System.nanoTime
    val result = block
    val t1 = System.nanoTime
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + "s")
    result
  }

  /**
   * An Object which composes of methods to compute near and exact duplicates
   */
  object DuplicateFinder {

    /**
     * A method that analyses the relevant content by converting it into a fingerprint and finds
     * whether the current content is exact or near duplicate of some fingerprint already
     * present in the frontier.
     * @param relevantContent The content seen by the user when he opens the html in a browser
     * @return the status (boolean) of the content that should be added into the frontier,
     *         true - not a exact duplicate and can be added
     *         false - duplicate and should not be added
     */
    def analyse(relevantContent : String): Boolean =
    {
      var Addflag = true

      val tokens = relevantContent.split("[ .,;:?!\t\n\r\f]+").toList
      //Shingles with q = 3
      val shingles = tokens.sliding(3)
      //A 32 bit hash is used
      val hashes = shingles.map(s => MurmurHash3.stringHash(s.mkString))
      def binary(value: Int) : String = String.format("%32s", Integer.toBinaryString(value))
        .replace(' ', '0')
      val hmap = hashes.map(h => binary(h))
      var sum = (for(i <- 1 to 32) yield 0).toList

      var length = 0
      hmap.foreach(s => {sum=add(sum,s); length = length+1})
      val finprint = sum.map(x => if(length/2 <= x) 1 else 0).toString()

      // Hamming distance = 0 => Exact Duplicate
      // Hamming distance <=2 => Near Duplicate (if not exact)
      breakable {
        for (x <- fingerprints) {
          val dis = hammingDistance(x, finprint)
          if (dis <= 2) {
            Addflag = false
            if (dis != 0)
              near_dup_counter = near_dup_counter + 1
            else {
              exact_dup_counter = exact_dup_counter + 1
              break
            }
          }
        }
      }

      if(Addflag) {
        fingerprints += finprint
      }

      return Addflag
    }

    /**
     * A method that takes strings 'sum' and 's' and compute sum of the contents in order
     * @param sum An integer list with the sums
     * @param s A string containing either 1 or 0
     * @return A list of integers with the computed sum
     */
    def add(sum:List[Int], s: String): List[Int] = {
      val charlist = s.toList
      return for( (x,y) <- (sum zip charlist)) yield if(y =='0') x else x+1

    }

    /**
     * The method Computes Hamming Distance between two strings composed of binary values
     * @param x
     * @param y
     * @return the hamming distance of strings x and y
     */
    def hammingDistance(x: String, y:String): Int =
    {
      val xChar = x.toList
      val yChar = y.toList
      var count = 0
      for((i,j) <- xChar zip yChar) if(i!=j) count = count+1
      return count
    }

  }
}
