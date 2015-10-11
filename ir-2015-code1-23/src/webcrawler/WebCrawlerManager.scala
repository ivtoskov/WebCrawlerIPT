package webcrawler

import scala.collection.mutable.{Map => MutMap}

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

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
    println("Visited pages: " + validLinksCount)
    println("Unique English pages found: " + englishCount)
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

      val relevantContent = section.text()
      val nonDuplicate = true// TODO Exact and near duplicates detection - Prabhu
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
}
