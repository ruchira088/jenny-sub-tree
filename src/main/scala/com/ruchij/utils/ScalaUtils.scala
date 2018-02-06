package com.ruchij.utils

object ScalaUtils
{
  def optionSet[A](options: Set[Option[A]]): Option[Set[A]] =
    options.toList match {
      case Nil => Some(Set.empty)
      case (x :: xs) => for {
        value <- x
        rest <- optionSet(xs.toSet)
      } yield rest + value
    }
}