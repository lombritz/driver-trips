
import java.time.{Instant, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.logging.Level

import com.gargoylesoftware.htmlunit.html._
import com.gargoylesoftware.htmlunit.{BrowserVersion, WebClient, WebClientOptions}
import org.apache.commons.logging.LogFactory

/**
 * @author Jaime Rojas
 * @date 5/11/2016
 */
case class Trip(time: LocalDateTime, driver: String, duration: Long, kilometers: Double, fare: Double, status: String)

object Main {

  val dateFormatter = DateTimeFormatter.ofPattern("MMMM d, yyyy h[:mm]a z")

  def main(args: Array[String]) {
    printTripsTable(enterUberPartner(args(0), args(1)))
  }

  def printTripsTable(uberPartnerPage: HtmlPage): Option[HtmlButton] = {
    println(uberPartnerPage.getUrl)
    val tables = uberPartnerPage.getElementsByTagName("table")
    if (tables.getLength > 0) {
      val tripsTable = Option(tables.get(tables.getLength - 1).asInstanceOf[HtmlTable])
      tripsTable match {
        case Some(table) => {
          val bodies = table.getBodies
          val links = bodies.get(0).getLastElementChild.getElementsByTagName("a")
          if (bodies.size() > 0 && links.getLength > 0) {
            val rows = bodies.get(0).getRows
            rows.subList(0, rows.size - 2).toArray.foreach { row =>
              val htmlRow = row.asInstanceOf[HtmlTableRow]

              val trip = Trip(
                LocalDateTime.parse(htmlRow.getCells.get(0).asText(), dateFormatter),
                htmlRow.getCells.get(1).asText(),
                timeInSeconds(htmlRow.getCells.get(2).asText().split(":")),
                htmlRow.getCells.get(3).asText() match {
                  case kilometers if kilometers.matches("^[0-9]+(\\.[0-9]+)?$") =>
                    kilometers.toDouble
                  case _ => 0d
                },
                htmlRow.getCells.get(4).asText().replaceAll("[A-z]+|[$]|,", "") match {
                  case distance if distance.matches("^[0-9]+(\\.[0-9]+)?$") =>
                    distance.toDouble
                  case _ => 0d
                },
                htmlRow.getCells.get(5).asText()
              )
              println(trip)
            }
            printTripsTable(links.get(0).asInstanceOf[HtmlAnchor].click().asInstanceOf[HtmlPage])
          } else None
        }
        case None => None
      }
    } else None
  }

  def timeInSeconds(time: Array[String]): Int = {
    if (time.length == 3) {
      time(0).toInt*3600 + time(1).toInt*60 + time(2).toInt
    } else if (time.length == 2) {
      time(0).toInt*60 + time(1).toInt
    } else if (time.length == 1 && "–" != time(0)) {
      time(0).toInt
    } else 0
  }

  def enterUberPartner(email: String, password: String): HtmlPage = {
    LogFactory.getFactory.setAttribute("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.NoOpLog");

    java.util.logging.Logger.getLogger("com.gargoylesoftware.htmlunit").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("org.apache.commons.httpclient").setLevel(Level.OFF)

    val browser: WebClient = new WebClient(BrowserVersion.CHROME)
    browser.getOptions.setCssEnabled(false)

    var partnerLogin: HtmlPage = browser.getPage("https://login.uber.com/login")
    partnerLogin = partnerLogin.getElementById("email").asInstanceOf[HtmlInput].setValueAttribute(email).asInstanceOf[HtmlPage]
    partnerLogin = partnerLogin.getElementById("password").asInstanceOf[HtmlInput].setValueAttribute(password).asInstanceOf[HtmlPage]
    val submitBtn: HtmlButton = partnerLogin.getElementsByTagName("button").get(0).asInstanceOf[HtmlButton]
    submitBtn.click()
  }
}