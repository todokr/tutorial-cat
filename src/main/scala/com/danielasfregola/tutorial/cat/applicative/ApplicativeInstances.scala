package com.danielasfregola.tutorial.cat.applicative

import com.danielasfregola.tutorial.cat._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object ApplicativeInstances {

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {

    override def pure[A](a: A): Just[A] = Just(a)
    override def ap[A, B](boxF: Maybe[A => B])(boxA: Maybe[A]): Maybe[B] = (boxF, boxA) match {
      case (Just(f), Just(a)) => pure(f(a))
      case _ => Empty
    }
  }

  implicit val zeroOrMoreApplicative: Applicative[ZeroOrMore] = new Applicative[ZeroOrMore] {
    override def pure[A](a: A): OneOrMore[A] = OneOrMore(a, Zero)
    override def ap[A, B](boxF: ZeroOrMore[(A) => B])(boxA: ZeroOrMore[A]): ZeroOrMore[B] = (boxF, boxA) match {
      case (OneOrMore(hF, _), OneOrMore(h, tail)) => OneOrMore(hF(h), ap(boxF)(tail))
      case _ => Zero
    }
  }
}
