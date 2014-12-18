package com.beardedcoder.mineExercise
package actions

import cuboid.Field._
import cuboid._
import movement._
import fire._
import Types._

sealed trait Action
case class Move(m: Movement) extends Action
case class Fire(p: FiringPattern) extends Action
case object Drop extends Action

object Actions {
  def process(action: Action, c: Cuboid, sp: ShipPosition): (Cuboid, ShipPosition) = {
    action match {
      case Move(m) => Movement.move(m, c, sp)
      case Fire(fp) => fireTorpedos(fp, c, sp)
      case Drop => dropZ(c, sp)
    }
  }

  def print(action: Action, c: Cuboid): Unit = {
    println
    println(action)
    println
    println(Field.cuboidString(c))
    println
    println("######################")
  }

  def addDrops(x: List[List[Action]]) : List[Action] = {
    x.reduceLeft { (a: List[Action], b: List[Action]) => a ++ List(Drop) ++ b }
  }

  def dropZ(c: Cuboid, sp: ShipPosition): (Cuboid, ShipPosition) = {
    val position = sp.copy(z = sp.z -1)
    val cuboid = updateMines(c, position)
    (cuboid, position)
  }

  def fireTorpedos(fp: FiringPattern, c: Cuboid, sp: ShipPosition): (Cuboid, ShipPosition) = {
    val cuboid = fp.p.foldRight(c) { (y: Coordinates2D, c2: Cuboid) => clearField(c2, y, sp)}
    (cuboid, sp)
  }

  def clearField(cu: Cuboid, co: Coordinates2D, sp: ShipPosition) : Cuboid = {
    try {
      val yaxis = cu(co.y + sp.co.y)
      val updatedYaxis = yaxis(co.x + sp.co.x) match {
        case Mine(_) => yaxis.updated(co.x + sp.co.x, Empty)
        case _ => yaxis
      }
      cu.updated(co.y + sp.co.y, updatedYaxis)
    } catch {
      case e: IndexOutOfBoundsException => cu //Trying to clear a field that doesn't exist
    }
  }

  def updateMines(cu: Cuboid, sp: ShipPosition): Cuboid = {
    val shipZ = sp.z
    cu.map { (vfs: Vector[FieldSpace]) =>
      vfs.map { (fs: FieldSpace) =>
        fs match {
          case Mine(m) => {
            if(-(shipZ) == m.id)
              MissedMine
            else
              Mine(MineDistance.fromDepth(m.id - 1))
          }
          case other => other
        }
      }
    }
  }
}

import scala.util.parsing.combinator._
object ScriptParser extends RegexParsers {
  import MineDistance._
  import Field._

  def north: Parser[Action] = "north" ^^ {(_: String) => Move(North)}
  def south: Parser[Action] = "south" ^^ {(_: String) => Move(South)}
  def east: Parser[Action] = "east" ^^ {(_: String) => Move(East)}
  def west: Parser[Action] = "west" ^^ {(_: String) => Move(West)}
  def alpha: Parser[Action] = """alpha""".r ^^ {(_: String) => Fire(FiringPattern.alpha)}
  def beta: Parser[Action] = """beta""".r ^^ {(_: String) => Fire(FiringPattern.beta)}
  def gamma: Parser[Action] = """gamma""".r ^^ {(_: String) => Fire(FiringPattern.gamma)}
  def delta: Parser[Action] = """delta""".r ^^ {(_: String) => Fire(FiringPattern.delta)}
  def eol: Parser[Any] = sys.props("line.separator")
  def eof: Parser[Any] = """\z""".r
  def space: Parser[String] = " "
  def movement: Parser[Action] = north | south | east | west
  def fire: Parser[Action] = alpha | beta | gamma | delta
  def combo: Parser[Action] = (fire <~ space) | (movement <~ space)
  def line: Parser[List[Action]] = (movement | fire ||| combo).* <~ eol

  def all: Parser[List[List[Action]]] = line.* <~ eof

  override def skipWhitespace = false

}
