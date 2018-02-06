package com.ruchij.utils

object ScalaUtils
{
  def optionSequence[A](optionList: List[Option[A]]): Option[List[A]] =
    optionList match {
      case Nil => Some(List.empty)
      case (x :: xs) => for {
        value <- x
        rest <- optionSequence(xs)
      } yield value :: rest
    }
}
