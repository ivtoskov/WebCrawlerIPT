package webcrawler

import scala.util.matching.Regex
import io.Source
import scala.collection.mutable.{Map => MutMap}

object LinkHelper {
  
  /* http regex
  private val linkRegex : Regex = """http://[A-Za-z0-9-_]+.[A-Za-z0-9-_:%&?/.=]+""".r */
  private val linkRegex : Regex = """href=\"[A-Za-z0-9-_]+.[A-Za-z0-9-_:%&?/.=]+\"""".r
  private val urlRegex = """(http|ftp|https)://(.*)\.([A-Za-z0-9-_:%&?/.=]+)""".r

 def splitURL(url : String): (String,String,String) = url match {
    case urlRegex(protocol, domain, tld) => (protocol, domain, tld)
  } 
  
  
  //searches for href items using regex
  def getLinks(html: String): List[String] = {
    linkRegex.findAllMatchIn(html).map(_.toString.replace("\"", "")).toList
    }
  
  //downloads page
  //returns page as string
  def readUrlContent(url: String) : String = {
    try{
      val in = Source.fromURL(url, "utf8")
      val response = in.getLines.mkString
      in.close()
      response
    }
    catch{
      case e: Exception => null
    }
    
  }
  
  
  def processLinks(links : List[String], domain: String, linkMap: MutMap[String, Boolean], domainLimit: String ) : MutMap[String, Boolean] = {
    links.foreach { link => {    
      var processedLink = link.replace("href=", "")
      //remove css hrefs
      if(!processedLink.endsWith(".css") && !processedLink.endsWith(".ico")&& !processedLink.endsWith(".jpg")
          && !processedLink.endsWith(".png"))
      {
        if((processedLink.startsWith("http://") || processedLink.startsWith("https://")))
        {
          if(!linkMap.exists(_._1 == processedLink) && processedLink.contains(domainLimit))
            linkMap += processedLink -> false
        }
        else{
          if(!linkMap.exists(_._1 == (domain + processedLink)))
            linkMap += (domain + processedLink) -> false
        }
      }}}
    linkMap
  }
  
  
}