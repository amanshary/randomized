package com.riskified.toolkit.test

import java.time.{Instant, LocalDate, OffsetDateTime, Year, ZoneOffset, ZonedDateTime}

import cats.data.{NonEmptyChain, NonEmptyList, NonEmptyVector}

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuild
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.Random

@implicitNotFound(
  "Missing implicit RandomObject for ${A}. Try calling RandomObject.gen[A] to see a detailed compilation error"
)
trait RandomObject[A] {
  def generate(): A
}

object RandomObject extends RandomObjectDerivation {

  def apply[A](implicit r: RandomObject[A]): A = r.generate()

  implicit val randomString: RandomObject[String] = () =>
    Random.alphanumeric.take(Random.nextInt(20)).mkString

  implicit val randomInt: RandomObject[Int] = () => Random.nextInt()

  implicit val randomLong: RandomObject[Long] = () => Random.nextLong()

  implicit val randomDouble: RandomObject[Double] = () => Random.nextDouble()

  implicit val randomFloat: RandomObject[Float] = () => Random.nextFloat()

  implicit val randomBigInt: RandomObject[BigInt] = () => BigInt(1024, Random)

  implicit val randomBigDecimal: RandomObject[BigDecimal] = () =>
    BigDecimal(randomBigInt.generate()) + BigDecimal(randomDouble.generate())

  implicit val randomBoolean: RandomObject[Boolean] = () => Random.nextBoolean()

  implicit val randomOffsetDateTime: RandomObject[OffsetDateTime] =
    () => OffsetDateTime.ofInstant(RandomObject[Instant], ZoneOffset.UTC)

  implicit val randomZonedDateTime: RandomObject[ZonedDateTime] =
    () => ZonedDateTime.ofInstant(RandomObject[Instant], ZoneOffset.UTC)

  implicit val randomDate: RandomObject[LocalDate] =
    () =>
      LocalDate.of(
        1 + Random.nextInt(Year.now().getValue),
        1 + Random.nextInt(12),
        1 + Random.nextInt(28)
      )

  implicit val randomFiniteDuration: RandomObject[FiniteDuration] =
    () => math.abs(Random.nextInt()).millis

  implicit val randomInstant: RandomObject[Instant] =
    () => Instant.now().minusMillis(Random.nextInt(1000000))

  implicit def randomColl[A: RandomObject, C[_]](
      implicit canBuild: CanBuild[A, C[A]]
  ): RandomObject[C[A]] =
    () => {
      val b = canBuild()
      val numberOfElems = Random.nextInt(10)
      b.sizeHint(numberOfElems)
      (0 to numberOfElems) foreach (_ ⇒ b += RandomObject[A])
      b.result()
    }

  implicit def randomTupleColl[A: RandomObject, B: RandomObject, C[_, _]](
      implicit canBuild: CanBuild[(A, B), C[A, B]]
  ): RandomObject[C[A, B]] =
    () => {
      val b = canBuild()
      val numberOfElems = Random.nextInt(10)
      b.sizeHint(numberOfElems)
      (0 to numberOfElems) foreach { _ ⇒
        val elem = (RandomObject[A], RandomObject[B])
        b += elem
      }
      b.result()
    }

  implicit def randomNec[A: RandomObject]: RandomObject[NonEmptyChain[A]] =
    () => NonEmptyChain.fromNonEmptyList(RandomObject[NonEmptyList[A]])

  implicit def randomNel[A: RandomObject]: RandomObject[NonEmptyList[A]] =
    () => NonEmptyList(RandomObject[A], RandomObject[List[A]])

  implicit def nevRnd[A: RandomObject]: RandomObject[NonEmptyVector[A]] =
    () => NonEmptyVector(RandomObject[A], RandomObject[Vector[A]])

  implicit def randomOption[A: RandomObject]: RandomObject[Option[A]] =
    () =>
      if (Random.nextBoolean())
        Some(implicitly[RandomObject[A]].generate())
      else None
}
