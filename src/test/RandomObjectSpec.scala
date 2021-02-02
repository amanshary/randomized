package com.riskified.toolkit.test

import java.time.{Instant, LocalDate, OffsetDateTime, ZonedDateTime}

import cats.data.{NonEmptyChain, NonEmptyList, NonEmptyVector}
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration
import scala.language.higherKinds

class RandomObjectSpec extends FlatSpec with Matchers {

  def nonEqualRandoms[A: RandomObject]: Assertion =
    assert(List.fill(10)(RandomObject[A]).toSet.size >= 2)

  "RandomObject" should "return random Int" in {
    nonEqualRandoms[Int]
  }
  it should "return random Float" in {
    nonEqualRandoms[Float]
  }
  it should "return random Double" in {
    nonEqualRandoms[Double]
  }
  it should "return random Long" in {
    nonEqualRandoms[Long]
  }
  it should "return random BigInt" in {
    nonEqualRandoms[BigInt]
  }
  it should "return random String" in {
    nonEqualRandoms[String]
  }
  it should "return random BigDecimal" in {
    nonEqualRandoms[BigDecimal]
  }
  it should "return random Instant" in {
    nonEqualRandoms[Instant]
  }
  it should "return random FiniteDuration" in {
    nonEqualRandoms[FiniteDuration]
  }
  it should "return random LocalDate" in {
    nonEqualRandoms[LocalDate]
  }
  it should "return random OffsetDateTime" in {
    nonEqualRandoms[OffsetDateTime]
  }
  it should "return random ZonedDateTime" in {
    nonEqualRandoms[ZonedDateTime]
  }

  case class T(x: Int, s: String)
  it should "return random T" in {
    nonEqualRandoms[T]
  }
  it should "return random List" in {
    nonEqualRandoms[List[T]]
  }
  it should "return random Vector" in {
    nonEqualRandoms[Vector[T]]
  }
  it should "return random NonEmptyList" in {
    nonEqualRandoms[NonEmptyList[T]]
  }
  it should "return random NonEmptyVector" in {
    nonEqualRandoms[NonEmptyVector[T]]
  }
  it should "return random NonEmptyChain" in {
    nonEqualRandoms[NonEmptyChain[T]]
  }
  it should "return random Boolean" in {
    nonEqualRandoms[Boolean]
  }

  sealed trait B
  case class BB(x: Int, s: String) extends B
  case class BBB(d: Double) extends B
  case class BBBB(d: Double, dd: Double) extends B
  case object C extends B
  case object CC extends B
  case object D extends B
  case object DD extends B
  case object E extends B
  case object EE extends B
  it should "return random sealed trait" in {
    nonEqualRandoms[B]
  }

  sealed trait F
  case class G(x: Int, s: String) extends F
  case class GG(x: Int, y: Int, s: String) extends F
  case class H(d: Double) extends F
  case class HH(f: Float) extends F
  case class I(b: G, s: String)
  case class II(bb: G, s: String)
  case class III(bbb: G, s: String)
  it should "return random composite object" in {
    nonEqualRandoms[I]
  }
}
