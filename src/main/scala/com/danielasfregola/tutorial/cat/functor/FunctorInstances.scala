package com.danielasfregola.tutorial.cat.functor

import com.danielasfregola.tutorial.cat._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object FunctorInstances {

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def map[A, B](m: Maybe[A])(f: A => B): Maybe[B] = m match {
      case Just(a) => Just(f(a))
      case Empty => Empty
    }
  }

  implicit val zeroOrMoreFunctor: Functor[ZeroOrMore] = new Functor[ZeroOrMore] {
    override def map[A, B](boxA: ZeroOrMore[A])(f: A => B): ZeroOrMore[B] = boxA match {
      case OneOrMore(h, t) => OneOrMore(f(h), map(t)(f))
      case Zero => Zero
    }
  }

}
