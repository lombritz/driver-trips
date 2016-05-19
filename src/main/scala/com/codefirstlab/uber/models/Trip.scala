package com.codefirstlab.uber.models

import java.time.{Duration, LocalDateTime}
import java.util.UUID

/**
  * @author Jaime Rojas
  * @date 5/11/2016
  */
case class Trip(uuid: UUID, time: LocalDateTime, driver: String, duration: Duration, kilometers: Double, fare: Double, status: String)
