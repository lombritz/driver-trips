
import java.time.{Duration, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.logging.Level

import com.gargoylesoftware.htmlunit.html._
import com.gargoylesoftware.htmlunit.{BrowserVersion, WebClient}
import org.apache.commons.logging.LogFactory

import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering
import scala.util.Try

/**
 * @author Jaime Rojas
 * @date 5/11/2016
 */
case class Trip(time: LocalDateTime, driver: String, duration: Duration, kilometers: Double, fare: Double, status: String)

object Main {
  val inputDateFormatter =  DateTimeFormatter.ofPattern("yyyyMMdd'T'HH:mm:ss'TZ'z")
  val uberDateFormatter = DateTimeFormatter.ofPattern("MMMM d, yyyy h[:mm]a z")

  def main(args: Array[String]) {
    val start = LocalDateTime.now()
    val uberPartnerHomePage = enterUberPartner(args(0), args(1))
    val fromDate = LocalDateTime.from(inputDateFormatter.parse(args(2)))
    val allTrips = loadTrips(uberPartnerHomePage, fromDate)

    implicit def localDateTimeComparator = Ordering.fromLessThan[LocalDateTime]((d1, d2) => d1.compareTo(d2) < 0)
    implicit def timeOrdering = Ordering.by((_: Trip).time)

    val avgKilometers = average(allTrips.map(t => t.kilometers))
    val avgDuration = Duration.ofSeconds(average(allTrips.map(t => t.duration.getSeconds)).toLong)
    val avgFare = average(allTrips.map(t => t.fare))

    val totalKilometers = allTrips.foldRight(0.0)((t, sum) => t.kilometers + sum)
    val totalDuration = Duration.ofSeconds(allTrips.foldRight(0L)((t, sum) => t.duration.getSeconds + sum))
    val totalFare = allTrips.foldRight(0.0)((t, sum) => t.fare + sum)

    val times = allTrips.map(_.time)
    println(
raw"""
trips: ${allTrips.size},
fromDateTime: $fromDate,
firstTrip: ${Try(times.min).getOrElse("")},
lastTrip: ${Try(times.max).getOrElse("")},
avgKilometers: $avgKilometers,
totalKilometers: $totalKilometers,
avgDuration: $avgDuration,
totalDuration: $totalDuration.,
avgFare: DOP $avgFare,
totalFare: DOP $totalFare
""")
    val end = LocalDateTime.now()

    val allSeconds = ChronoUnit.SECONDS.between(start, end)
    val minutes = allSeconds / 60
    val seconds = allSeconds % 60

    println(s"execution time: ${minutes}m ${seconds}s")
  }

  def loadTrips(uberPartnerPage: HtmlPage, from: LocalDateTime): Seq[Trip] = {
    def loadTripsHelper(uberPartnerPage: HtmlPage, trips: Seq[Trip]): Seq[Trip] = {
      getNextButton(uberPartnerPage) match {
        case Some(btn)  =>
          val rawPageTrips = getPageTrips(uberPartnerPage)
          rawPageTrips match {
            case Nil => trips
            case xs: Seq[Trip] =>
              // FIXME: time filtering (~8 hours difference).
              xs.filter (t => t.time.toEpochSecond(ZoneOffset.UTC) > from.toEpochSecond(ZoneOffset.UTC)) match {
                case fs: Seq[Trip] if fs.size < xs.size => trips ++ fs
                case _ => loadTripsHelper(btn.click(), trips ++ xs)
              }
          }
        case _ => trips
      }
    }
    loadTripsHelper(uberPartnerPage, Nil)
  }

  def getNextButton(uberPartnerPage: HtmlPage): Option[HtmlElement] = {
    val tables = uberPartnerPage.getElementsByTagName("table")
    if (tables.getLength > 0) {
      val tripsTable = Option(tables.get(tables.getLength - 1).asInstanceOf[HtmlTable])
      tripsTable match {
        case Some(table) =>
          val bodies = table.getBodies
          if (bodies.size() > 0) {
            val links = bodies.get(0).getLastElementChild.getElementsByTagName("a")
            if (links.getLength > 0) {
              val link = links.get(0)
              link.asText() match {
                case "Next" =>
                  Some(link)
                case _ => None
              }
            } else None
          } else None
        case _ => None
      }
    } else None
  }

  def getPageTrips(uberPartnerPage: HtmlPage): Seq[Trip] = {
    val tables = uberPartnerPage.getElementsByTagName("table")
    if (tables.getLength > 0) {
      val tripsTable = Option(tables.get(tables.getLength - 1).asInstanceOf[HtmlTable])
      tripsTable match {
        case Some(table) =>
          val bodies = table.getBodies
          if (bodies.size() > 0) {
            val rows = bodies.get(0).getRows
            val pageTrips = ArrayBuffer.empty[Trip]
            rows.subList(0, rows.size - 1).toArray.foreach { row =>
              val htmlRow = row.asInstanceOf[HtmlTableRow]
              pageTrips += Trip(
                LocalDateTime.parse(htmlRow.getCells.get(0).asText(), uberDateFormatter),
                htmlRow.getCells.get(1).asText(),
                Duration.ofSeconds(timeInSeconds(htmlRow.getCells.get(2).asText().split(":"))),
                Try(htmlRow.getCells.get(3).asText().toDouble).getOrElse(0.0),
                Try(htmlRow.getCells.get(4).asText().replaceAll("[A-z]+|[$]|,", "").toDouble).getOrElse(0.0),
                htmlRow.getCells.get(5).asText()
              )
            }
            pageTrips.filter(_.status == "Completed")
          } else Nil
        case _ => Nil
      }
    } else Nil
  }

  def average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
    num.toDouble( ts.sum ) / ts.size
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
    LogFactory.getFactory.setAttribute("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.NoOpLog")

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