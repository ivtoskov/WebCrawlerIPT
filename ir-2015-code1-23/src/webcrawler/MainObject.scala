package webcrawler

/**
 * @author Stefan
 */
object MainObject {
  def main(args: Array[String])
  {
    var linkArg : String = null
    if(args.length > 0)
      linkArg = args.apply(0)
    val crawler = new WebCrawlerManager()
    crawler.run()
  
  }
}