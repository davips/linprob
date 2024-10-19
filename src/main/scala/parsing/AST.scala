// Copyright (c) 2020. Davi Pereira dos Santos
//     This file is part of the tupi project.
//     Please respect the license. Removing authorship by any means
//     (by code make up or closing the sources) or ignoring property rights
//     is a crime and is unethical regarding the effort and time spent here.
//     Relevant employers or funding agencies will be notified accordingly.
//
//     tupi is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     tupi is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with tupi.  If not, see <http://www.gnu.org/licenses/>.
//
package parsing

//import algebra.Hosh
import inference.Types.EmptyT
import runtime.LMap

// import scala.reflect.runtime.currentMirror
// import scala.tools.reflect.ToolBox

object AST {

  import inference.Types.ExprT

  var m: LMap[Expr] = LMap()

  sealed trait Expr {
    var t: Option[ExprT] = None
    //val hosh: Option[Hosh]
    def nested: Iterator[Expr]
  }

  case class PartialOp(opsymbol: String) extends Expr {
    override val toString = f"{$opsymbol}"
    def nested: Iterator[Expr] = Iterator.empty
  }

  case class OpTo(opsymbol: String, value: Num) extends Expr {
    override val toString = f"{$opsymbol}($value)"
    def nested: Iterator[Expr] = Iterator.empty
  }

  sealed trait PrimitiveExpr extends Expr {
    def value: Any
    //lazy val hosh: Option[Hosh] = Some(Hosh(getClass.toString.map(_.toByte).toArray) * Hosh(toString.map(_.toByte).toArray))
  }

  object PrimitiveExpr {
    def unapply(expr: PrimitiveExpr): Option[Any] = Some(expr.value)
    def apply(value: Any): PrimitiveExpr = value match {
      case v: Boolean => Bool(v)
      case v: Double => Num(v)
      //      case v: java.lang.Boolean => Bool(v)
      //      case v: java.lang.Double => Num(v)
      case v: Character => Char(v)
      case v: String => Text(v)
    }
  }

  case class Empty() extends PrimitiveExpr {
    lazy val value: Any = this // Era lazy val value: Empty = this
    override val toString = "ø"
    t = Some(EmptyT)
    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Bool(value: java.lang.Boolean) extends PrimitiveExpr {
    override val toString: String = if (value) "↑" else "↓"
    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Char(value: java.lang.Character) extends PrimitiveExpr {
    override val toString: String = f"'$value'"
    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Num(value: Double) extends PrimitiveExpr {
    override val toString: String = value.toString
    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Text(value: String) extends PrimitiveExpr {
    override lazy val toString: String = "\"" + value + "\""
    def nested: Iterator[Expr] = Iterator.empty
  }

  //  case class Op(f: (a: Double) => (b: Double) => Double, value: Expr) extends PrimitiveExpr {
  //    override lazy val toString: String = "\"f→" + value + "\""
  //    def nested: Iterator[Expr] = Iterator.empty
  //  }

  case class Closure(value: Lambda | PartialOp | OpTo, ctx: LMap[Expr]) extends PrimitiveExpr {
    override val toString: String = value.t.getOrElse("«undefined function type»").toString
    //override lazy val hosh: Option[Hosh] = ??? // it probably makes no sense to have a hosh here
    def nested: Iterator[Expr] = Iterator.empty //TODO: check this
  }

  case class Lambda(param: Ident, body: Sequence) extends Expr {
    //lazy val paramHosh: Hosh = Hosh(param.name.map(_.toByte).toArray)
    override val toString: String = f"λ$param→$body"
    //lazy val hosh: Option[Hosh] = Some(Hosh(getClass.toString.map(_.toByte).toArray) * paramHosh * body.hosh.get)
    def nested: Iterator[Expr] = body.nested
  }

  case class Assign(a: NamedIdent, b: Expr) extends Expr {
    m = m.put(a.name, b)
    override val toString: String = String.valueOf(a) + "←" + b
    //lazy val hosh: Option[Hosh] = Some(Hosh(getClass.toString.map(_.toByte).toArray) * a.hosh.get * b.hosh.get)
    def nested: Iterator[Expr] = Iterator(a, b)
  }

  case class Appl(a: Expr, b: Expr) extends Expr {
    override val toString: String = String.valueOf(a) + "(" + b + ")"
    //     lazy val hosh: Option[Hosh] = (Some(
    //       Hosh(getClass.toString.map(_.toByte).toArray) *
    //         a.hosh.get *
    //         b.hosh.get
    //     ))
    def nested: Iterator[Expr] = Iterator(a, b)
  }

  trait Ident extends Expr {
    val name: String
    lazy val expr: Option[Expr] = {
      println(name + m)
      m.get(name)
    }
    override def toString: String = name
    //     lazy val hosh: Option[Hosh] = expr match {
    //       case Some(expr) => expr.hosh
    //       case None => None
    //     }
    def nested: Iterator[Expr] = Iterator.empty
  }

  object Ident {
    def unapply(e: Ident): Option[String] = Some(e.name)
  }

  case class NamedIdent(name: String) extends Ident

  case class AnonIdent() extends Ident {
    val name: String = "_"
  }

  case class AnonIdentN(idx: Int) extends Ident {
    val name: String = "_" + idx
  }

  case class Sequence(items: List[Expr]) extends Expr {
    override val toString: String = "(" + items.mkString("; ") + ")"
    //lazy val hosh: Option[Hosh] = if (items.size == 1) items.head.hosh else Some(items.dropRight(1).map(_.hosh.get).sortBy(_.n).reduce(_ * _) * items.last.hosh.get)
    def nested: Iterator[Expr] = items.iterator
  }

  case class Id() extends Expr {
    override val toString: String = "#"
    //lazy val hosh: Option[Hosh] = Some(Hosh(getClass.toString.map(_.toByte).toArray))
    def nested: Iterator[Expr] = Iterator.empty
  }

  //   case class Scala(params: List[NamedIdent], code: Text) extends Expr {
  //     //val paramsHosh: Hosh = params.map(param => Hosh(param.name.map(_.toByte).toArray)).reduce(_ * _)
  //     //override val toString: String = "«" + params.mkString(",") + ": " + code + "»"
  //     //lazy val hosh: Option[Hosh] = Some(Hosh(getClass.toString.map(_.toByte).toArray) * paramsHosh * code.hosh.get)
  //     def func(args: List[Any]): PrimitiveExpr = {
  //       val toolbox = currentMirror.mkToolBox()
  //       val vars = params.zipWithIndex.map {
  //         case (i@Ident(name), idx) => f"  val $name = args($idx).asInstanceOf[${i.t.get.scalaTypeDescr}]\n"
  //       }
  //       //      println("111111111111111111111", code.value)
  //       val txt =
  //         f"""def f(args: List[Any]) = {
  //            |${vars.mkString}""".stripMargin + "  " + code.value +
  //           f"""
  //              |}
  //              |f(List(${args.mkString(", ")}))""".stripMargin
  //       //      println(txt)
  //       val evaluated = toolbox.eval(toolbox.parse(txt))
  //       PrimitiveExpr(evaluated)
  //     }
  //     def nested: Iterator[Expr] = Iterator.empty
  //   }
}
