package com.beardedcoder.mineExercise
package movement

sealed trait Movement
case object North extends Movement
case object South extends Movement
case object East extends Movement
case object West extends Movement

case class ShipPosition(co: Coordinates2D, z: Int)
case class Coordinates2D(x: Int, y: Int)


object Movement {
  import cuboid._
  import Types._

  //NOTE: Recalculating middle every move as my previous mechanism had bugs
  def move(move: Movement, cube: Cuboid, sp: ShipPosition): (Cuboid, ShipPosition) = move match {
    case North => {
      val emptySpace = Vector.fill(cube.head.length)(Empty)
      val cuboid = shrinkY(emptySpace +: emptySpace +: cube)
      val newPos = ShipPosition(middle(cuboid), sp.z)
      (cuboid, newPos)
    }
    case South => {
      val emptySpace = Vector.fill(cube.head.length)(Empty)
      val cuboid = shrinkY(cube :+ emptySpace :+ emptySpace)
      val newPos = ShipPosition(middle(cuboid), sp.z)
      (cuboid, newPos)
    }
    case East => {
      val emptySpace = Vector.fill(2)(Empty)
      val cuboid = shrinkX(cube.map {(cu: Vector[FieldSpace]) => cu ++ emptySpace})
      val newPos = ShipPosition(middle(cuboid), sp.z)
      (cuboid, newPos)
    }
    case West => {
      val emptySpace = Vector.fill(2)(Empty)
      val cuboid = shrinkX(cube.map {(cu: Vector[FieldSpace]) => emptySpace ++ cu})
      val newPos = ShipPosition(middle(cuboid), sp.z)
      (cuboid, newPos)
    }
  }

  def shrinkX(cube: Cuboid) : Cuboid = {
    //Prevent shrinking past 1 wide
    if(cube.head.length == 1) {
      cube
    } else {
      val l = cube.map(_.head)
      val r = cube.map(_.last)
      if (l.forall((x) => x == Empty) && r.forall((x) => x == Empty))
        shrinkX(cube.map(_.drop(1).dropRight(1))) else cube
    }
  }

  def shrinkY(cube: Cuboid) : Cuboid = {
    if(cube.length == 1) {
      cube
    } else {
      if(cube.head.forall((x: FieldSpace) => x == Empty) &&
        cube.last.forall((x: FieldSpace) => x == Empty)) shrinkY(cube.drop(1).dropRight(1)) else cube
    }

  }

  def startingPos(cube: Cuboid): ShipPosition = {
    ShipPosition(middle(cube), 0)
  }

  def middle(cube: Cuboid) : Coordinates2D = {
    val y = if(cube.length < 3) 0 else cube.length / 2
    val x = if(cube.head.length < 3) 0 else cube.head.length / 2
    Coordinates2D(x,y)
  }

}
