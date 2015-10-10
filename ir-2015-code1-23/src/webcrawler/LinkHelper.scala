package webcrawler

import scala.util.matching.Regex
import io.Source
import scala.collection.mutable.{Map => MutMap}

object LinkHelper {
  // A regular expression that matches links in an HTML document
  private val linkRegex : Regex = """href=\"[A-Za-z0-9-_]+.[A-Za-z0-9-_:%&?/.=]+\"""".r
  // A regular expression that matches URLs
  private val urlRegex = """(http|ftp|https)://(.*)\.([A-Za-z0-9-_:%&?/.=]+)""".r

  /**
   * Splits the given url and returns its domain
   *
   * @param url The URL to be split
   * @return The domain of the given URL
   */
  def splitURL(url : String): String = url match {
    case urlRegex(protocol, domain, tld) => domain
  }


  /**
   * Scans provided HTML document for links using regex
   *
   * @param html The HTML document to be scanned, given as a String
   * @return List of all links contained in the document
   */
  def getLinks(html: String): List[String] = {
    linkRegex.findAllMatchIn(html).map(_.toString().replace("\"", "")).toList
  }

  /**
   * Downloads a page from a given url and returns its content as a String
   *
   * @param url The URL to be downloaded
   * @return The content of the URL as a String
   */
  def readUrlContent(url: String) : String = {
    try {
      val in = Source.fromURL(url, "utf8")
      val response = in.getLines().mkString
      in.close()
      response
    }
    catch {
      case e: Exception => null
    }
  }

  /**
   * Scans through all the candidate links and filters out
   * those that are not eligible to be added to the frontier.
   *
   * @param links Candidate links for the frontier
   * @param domain The base domain of the current link
   * @param linkMap Map containing already visited links
   * @param domainLimit The domain that should not be left
   */
  def processLinks(links : List[String], domain: String, linkMap: MutMap[String, Boolean], domainLimit: String ) : Unit = {
    links.foreach {
      link => {
        var processedLink = link.replace("href=", "")
        if(processedLink.lastIndexOf('#') != -1)
          processedLink = processedLink.substring(0, processedLink.lastIndexOf('#'))
        if(processedLink.lastIndexOf('?') != -1)
          processedLink = processedLink.substring(0, processedLink.lastIndexOf('?'))
        if(!processedLink.endsWith(".css") && !processedLink.endsWith(".ico") && !processedLink.endsWith(".jpg")
            && !processedLink.endsWith(".png")) {
          if(processedLink.startsWith("http://") || processedLink.startsWith("https://")) {
            if(!linkMap.exists(_._1 == processedLink) && processedLink.contains(domainLimit))
              linkMap += processedLink -> false
          } else {
            if(!linkMap.exists(_._1 == (domain + processedLink)))
              linkMap += (domain + processedLink) -> false
          }
        }
      }
    }
  }
}
