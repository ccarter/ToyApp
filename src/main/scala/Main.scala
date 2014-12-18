package com.beardedcoder.mineExercise

import movement._

object Main {
  def main(args: Array[String]) = {
    val field = scala.io.Source.fromFile("field").mkString
    val script = scala.io.Source.fromFile("script").mkString
    App.run(field, script)
    println("Done")
  }
}

object App {
  import cuboid._
  import actions._
  import actions.Actions._
  import Field._
  import Types._
  import movement.Movement.startingPos

  def run(fieldIn: String, scriptIn: String) = {
    val script = addDrops(ScriptParser.parseAll(ScriptParser.all, scriptIn).get).reverse
    val startingCuboid = toVector(FieldParser.parseAll(FieldParser.all, fieldIn).get)
    val startingPosition = startingPos(startingCuboid)

    println("### STARTING FIELD ###")
    println(Field.cuboidString(startingCuboid))
    println("######################")

    script.foldRight((startingCuboid, startingPosition)){
      (a: Action, s: (Cuboid,ShipPosition)) => {
        val (newCuboid, newPosition) = Actions.process(a, s._1, s._2)
        Actions.print(a, newCuboid)
        (newCuboid, newPosition)
      }
    }
  }
}
