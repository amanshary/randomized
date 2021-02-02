package com.riskified.toolkit.test

import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.language.experimental.macros
import scala.util.Random

trait RandomObjectDerivation {

  type Typeclass[A] = RandomObject[A]

  def combine[A](caseClass: CaseClass[Typeclass, A]): Typeclass[A] =
    () => caseClass.construct(p => p.typeclass.generate())

  private def choice[A](xs: Seq[A]): A =
    xs.toIndexedSeq(Random.nextInt(xs.length))

  def dispatch[A](sealedTrait: SealedTrait[Typeclass, A]): Typeclass[A] =
    () => choice(sealedTrait.subtypes).typeclass.generate()

  implicit def gen[A]: Typeclass[A] = macro Magnolia.gen[A]
}
