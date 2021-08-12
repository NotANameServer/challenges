package io.github.iltotore.powerfour

import scala.io.StdIn

object Eithers {

  /**
   * Either-based way to read an int from StdIn.
   * @param msg the prompt message
   * @param left the left result if the input is invalid
   * @tparam A the left type
   * @return Right(Int) if the user input is a valid integer. Left(left) otherwise.
   */
  def readInt[A](msg: String, left: => A): Either[A, Int] = StdIn.readLine(msg)
    .trim
    .replace("\n", "")
    .toIntOption
    .toRight(left)

  /**
   * Re-evaluate `either` until Right.
   * @param either the Either to evaluate each try
   * @param doLeft the action applied to Left when the Either isn't yet right
   * @tparam A left type
   * @tparam B right type
   * @return the result of the Right once the input is valid
   */
  def waitRight[A, B](either: => Either[A, B])(doLeft: A => Unit): B = {
    var result = either
    while(result.isLeft) {
      result.left.foreach(doLeft)
      result = either
    }
    result.getOrElse(???)
  }
}