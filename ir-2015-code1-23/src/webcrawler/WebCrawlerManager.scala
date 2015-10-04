package webcrawler

import io.Source
import scala.util.matching.Regex
import scala.collection.mutable.{Map => MutMap}

class WebCrawlerManager(val baseUrl: String = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html") {
  
  var linkMap : MutMap[String,Boolean] = null
  var domainLimit = ""
  
  def run(){
    val(protocol, domain, tld) = LinkHelper.splitURL(baseUrl)
    val baseDomain =  protocol + "://" + domain.substring(0, domain.lastIndexOf("/") + 1)
    domainLimit = domain.substring(0, domain.lastIndexOf("www"))
    println("base domain: " + domainLimit)
    
    linkMap = MutMap[String, Boolean]()  
    linkMap += baseUrl -> false
    
    
    //do initializations and other stuff
    time {this.crawlEntry()}    
  
  }
  
  private def crawlEntry()
  {
    var linksRemaining = true
    while(linksRemaining)
    {
      val nextLink = linkMap.find(p => p._2 == false).getOrElse(null)
      if(nextLink!= null)
      {
      crawl(nextLink._1, nextLink._1.substring(0, nextLink._1.lastIndexOf("/") + 1))
      }
      else
      {
        linksRemaining = false
      }
    }
    //Number of links: 9576
    println("CRAWLER FINISHED")
  }
  
  //crawls recursively on all links
  //entry point for all other processing
  private def crawl(url : String, domain: String){
    linkMap(url) = true
    println("Current link: " + url)
    //THIS IS WHERE YOU GET PAGE HTML
    val pageContents = LinkHelper.readUrlContent(url)
    if(!(pageContents == null))
    {
    //extract href elements from html
    LinkHelper.processLinks(LinkHelper.getLinks(pageContents), domain, linkMap, domainLimit)
    //write links to file or something
    println("Number of links: " + linkMap.size)
    }
    
  }
  
  private def time[R](block: => R): R = {
    val t0 = System.nanoTime
    val result = block    
    val t1 = System.nanoTime
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + "s")
    result
}
 
}