package webcrawler

/**
 * @author Stefan
 */
object MainObject {
  def main(args: Array[String]) {
    var linkArg : String = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    if(args.length > 0)
      linkArg = args.apply(0)
    val crawler = new WebCrawlerManager(linkArg)
    crawler.run()
  }
}
