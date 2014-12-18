package com.beardedcoder.mineExercise
package fire

import movement._

trait FiringPattern {
  val p: List[Coordinates2D]
}
//NOTE: I don't really like this, but constrained for time
case class Alpha(p: List[Coordinates2D]) extends FiringPattern { override def toString() = "alpha"}
case class Beta(p: List[Coordinates2D]) extends FiringPattern { override def toString() = "beta"}
case class Gamma(p: List[Coordinates2D]) extends FiringPattern { override def toString() = "gamma"}
case class Delta(p: List[Coordinates2D]) extends FiringPattern { override def toString() = "delta"}

object FiringPattern {
  val alpha = Alpha(List(
    Coordinates2D(-1,-1),
    Coordinates2D(-1,1),
    Coordinates2D(1,-1),
    Coordinates2D(1,1)))

  val beta = Beta(List(
    Coordinates2D(-1,0),
    Coordinates2D(0,-1),
    Coordinates2D(0,1),
    Coordinates2D(1,0)))

  val gamma = Gamma(List(
    Coordinates2D(-1,0),
    Coordinates2D(0,0),
    Coordinates2D(1,0)))

  val delta = Delta(List(
    Coordinates2D(0,-1),
    Coordinates2D(0,0),
    Coordinates2D(0,1)))
}
