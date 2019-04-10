package me.heaton.typeclass

import java.util.Date

object Invariants {

  import cats._
  import cats.implicits._

  def longToDate: Long => Date = new Date(_)

  def dateToLong: Date => Long = _.getTime

  implicit val semigroupDate: Semigroup[Date] =
    Semigroup[Long].imap(longToDate)(dateToLong)

}
