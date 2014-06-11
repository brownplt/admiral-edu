package admiraledu.submit

import java.net.{URI, CookieHandler}
import java.nio.file.{Path, Paths, Files}
import dispatch.{Future => _, _}
import scala.concurrent._
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import ExecutionContext.Implicits.global
import com.ning.http.client.{FilePart, StringPart}

object Main extends App {

  val cookieManager = new HackedCookieManager(
    new URI("https://cs220.cs.umass.edu"),
    Paths.get(System.getProperty("user.home"), ".submit-cookie"))

  CookieHandler.setDefault(cookieManager)

  val isLoggedInUrl = "https://cs220.cs.umass.edu/ct/review"
  val uploadUrl = "https://cs220.cs.umass.edu/ct/"

  def login() : Future[Unit] = async {

    val req = cookieManager.urlWithCookies(isLoggedInUrl)
    val isLoggedIn = await(Http(req))
    isLoggedIn.getStatusCode() match {
      case 200 => println("Logged in")
      case 302 => {
        println("Session expired. You need to login...")
        cookieManager.setCookie(isLoggedInUrl, isLoggedIn.getCookies())
        LoginWindow.show(isLoggedIn.getHeader("Location"), isLoggedInUrl)
        println("Trying again...")
        login()
      }
      case code => println(s"Error: unexpected code ($code)")
    }
  }

  def submit(asgn : String, step : String, path : String) : Future[Unit] = async {
    import scala.sys.process._

    await(login())

    val tar = Files.createTempFile(s"$asgn-$step", "tar").toFile()
    val tarProc = s"tar c $path" #> tar
    tarProc.! match {
      case 0 => {
        val req = cookieManager.urlWithCookies(isLoggedInUrl)
          .addBodyPart(new StringPart("assignment", asgn))
          .addBodyPart(new StringPart("step", step))
          // empty string is the charset below
          .addBodyPart(new FilePart("file", tar, "application/x-tar", ""))
          .POST
        println(await(Http(req)).getStatusCode())
      }
      case code => {
        println(s"Error: could not create tar file (exit code $code)")
      }
    }
  }

  args match {
    case Array("submit", asgn, step, path) => {
      Await.result(submit(asgn, step, path), Duration.Inf)
    }
    case _ => 
      println("Invalid arguments. See source code for help.")
  }

}