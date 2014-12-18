package com.beardedcoder.mineExercise
package cuboid

object Field {
  import MineDistance._
  import Types._

  sealed trait FieldSpace

  case object Empty extends FieldSpace {
    override def toString() = "."
  }

  case object MissedMine extends FieldSpace {
    override def toString() = "*"
  }

  case class Mine(md: MineDistance) extends FieldSpace {
    override def toString() = md.toString
  }

  def cuboidString(cs: Seq[Seq[FieldSpace]]): String = {
    cs.map { (c) =>
      c.map(_.toString).mkString("")
    }.mkString("\n")
  }

  def cuboidEmpty(cu: Cuboid): Boolean = cu.forall {(vfs) => vfs.forall((x) => x == Empty)}

  def allBombsPast(cu: Cuboid): Option[Int] = {
    val (missed, left) = cu.foldRight((0,0)) {(vfs, m) =>
      vfs.foldRight(m) {(y, m2) => y match {
        case _ => (1,  1)
      }}}
    if(left > 0) None else Some(missed)
  }

  def toVector(fs: List[List[FieldSpace]]) : Cuboid = {
    fs.map(_.to[Vector]).to[Vector]
  }


}

object MineDistance extends Enumeration(1) {
  type MineDistance = Value
  val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z = Value

  def toDepth(md: MineDistance) : Int = md.id
  def fromDepth(depth: Int) : MineDistance =  MineDistance(depth)
  def fromString(r: String) : MineDistance = MineDistance.withName(r)
}

import scala.util.parsing.combinator._
object FieldParser extends RegexParsers {
  import MineDistance._
  import Field._
  def mine: Parser[FieldSpace] = """[a-zA-Z]""".r ^^ { (x: String) => Mine(MineDistance.fromString(x))}
  def empty: Parser[FieldSpace] = "." ^^ { (_: String) => Empty }
  def eol: Parser[Any] = sys.props("line.separator")
  def eof: Parser[Any] = """\z""".r
  def line: Parser[List[FieldSpace]] = (mine | empty).* <~ eol
  def all: Parser[List[List[FieldSpace]]] = line.* <~ eof

  override def skipWhitespace = false

}
