package admiraledu.submit

import java.net.{URI, CookieManager}
import java.nio.file.{Path, Paths, Files}
import spray.json._
import java.util.{HashMap, LinkedList}

private case class Cookies(cookies : List[String])

private object JsonProtocol extends DefaultJsonProtocol {

  implicit val cookiesFormat = jsonFormat1(Cookies)

}

// Saves the latest Set-Cookie headers with the same scheme:host:port as
// base to storage. This is not robust. E.g., if two requests to the
// same scheme:host:port return two different sets of cookies, it only
// stores the latest set. However, if a request returns an empty set of
// cookies, it is ignored.
class HackedCookieManager(base : URI, storage : Path) extends CookieManager {

  import JsonProtocol._

  private val cookies = new com.sun.webkit.network.CookieManager()

  if (Files.exists(storage)) {

    val cookiesStr = new String(Files.readAllBytes(storage))
    val cookiesJson = cookiesStr.parseJson.convertTo[Cookies]
    this.setCookie(base, cookiesJson.cookies)
  }

  type Headers = java.util.Map[String, java.util.List[String]]

  

  def setCookie(uri : URI, cookie : List[String]) : Unit = {
    import scala.collection.JavaConversions._
    val map = new HashMap[String, java.util.List[String]]()
    map.put("Set-Cookie", cookie)
    cookies.put(uri, map)
  }

  def setCookie(uri : String, cookies : java.util.List[_]) : Unit = {
    import scala.collection.JavaConversions._
    setCookie(new URI(uri), cookies.toList.map(_.toString))
  }

  def urlWithCookies(base : String) : dispatch.Req = {
    import scala.collection.JavaConversions._
    val hashMap = cookies.get(new URI(base), new HashMap[String, java.util.List[String]]())
    val cs = hashMap.getOrElse("Cookie", new LinkedList[String]()).toList
    cs.foldLeft(dispatch.url(base)) ((req, cookie) => req.addHeader("Cookie", cookie))
  }

  override def get(uri : URI, requestHeaders : Headers) : Headers = {
    cookies.get(uri, requestHeaders)
  }

  override def put(uri : URI, responseHeaders : Headers) : Unit = {
    import scala.collection.JavaConversions._

    if (base.getHost() == uri.getHost() &&
        base.getScheme() == uri.getScheme() &&
        base.getPort() == uri.getPort()) {
      responseHeaders.toMap.get("Set-Cookie") match {
        case Some(cookies) => {
          val cookiesJson = Cookies(cookies.toList).toJson
          Files.write(storage, cookiesJson.prettyPrint.getBytes)
        }
        case None => ()
      }
    }
    cookies.put(uri, responseHeaders)
  }



}
