package com.codefirstlab.uber.tests

import java.time.{Duration, LocalDateTime}
import java.time.temporal.ChronoUnit

import com.codefirstlab.uber.models.Trip
import com.codefirstlab.uber.services.UberAccountService
import org.scalatest.FlatSpec

import scala.util.Try

/**
  * Created by jaime on 5/15/16.
  */
class UberAccountServiceSpec extends FlatSpec {

  "A UberAccountService" should "get a Seq of all trips from a specific date" in {

    val start = LocalDateTime.now()
    val from = LocalDateTime.of(2016, 5, 16, 4, 0)

    val service = new UberAccountService("ubersti2@gmail.com", "Ubersti234")
    val allTrips = service.allTrips(from)

    allTrips.foreach(println)

    val tripsNo = allTrips.size.toDouble

    implicit def localDateTimeComparator = Ordering.fromLessThan[LocalDateTime]((d1, d2) => d1.compareTo(d2) < 0)
    implicit def timeOrdering = Ordering.by((_: Trip).time)

    val totalKilometers = allTrips.foldRight(0.0)((t, sum) => t.kilometers + sum)
    val totalDuration = allTrips.foldRight(0L)((t, sum) => t.duration.getSeconds + sum)
    val totalFare = allTrips.foldRight(0.0)((t, sum) => t.fare + sum)
    val uberFee = totalFare * 0.25

    val avgKilometers = totalKilometers / tripsNo
    val avgDuration = totalDuration / tripsNo
    val avgFare = totalFare / tripsNo

    val times = allTrips.map(_.time)

    val end = LocalDateTime.now()

    val allSeconds = ChronoUnit.SECONDS.between(start, end)


    import java.text.DecimalFormat
    val formatter = new DecimalFormat("#,###,###.##")

    println(raw"""
UBER ACCOUNT STATISTICS

No. of Trips:       ${allTrips.size}
From Date:          $from
First Trip Date:    ${Try(times.min).getOrElse("")}
Last Trip Date:     ${Try(times.max).getOrElse("")}
Avg. Kilometers:    ${formatter.format(avgKilometers)} KM
Avg. Duration:      ${Duration.ofSeconds(avgDuration.toLong)}
Avg. Fare:          DOP ${formatter.format(avgFare)}
Total Kilometers:   ${formatter.format(totalKilometers)} KM
Total Duration:     ${Duration.ofSeconds(totalDuration.toLong)}
Total Gross Fare:   DOP ${formatter.format(totalFare)}
Uber Fee (25%):     DOP ${formatter.format(uberFee)}
Total Net Fare:     DOP ${formatter.format(totalFare - uberFee)}

Execution Time: ${Duration.ofSeconds(allSeconds)}
""")
  }

}
