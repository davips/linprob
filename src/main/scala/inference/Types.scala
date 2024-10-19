/*
 * http://dysphoria.net/code/hindley-milner/HindleyMilner.scala
 * Andrew Forrest
 *
 * Implementation of basic polymorphic type-checking for a simple language.
 * Based heavily on Nikita Borisov’s Perl implementation at
 * http://web.archive.org/web/20050420002559/www.cs.berkeley.edu/~nikitab/courses/cs263/hm.html
 * which in turn is based on the paper by Luca Cardelli at
 * http://lucacardelli.name/Papers/BasicTypechecking.pdf
 *
 * If you run it with "scala HindleyMilner.scala" it will attempt to report the types
 * for a few example expressions. (It uses UTF-8 for output, so you may need to set your
 * terminal accordingly.)
 *
 */

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

package inference

import parsing.AST.*

object Types {

  sealed trait ExprT {
    lazy val scalaTypeDescr: String
  }

  case object EmptyT extends ExprT {
    override def toString = "'ø'"

    lazy val scalaTypeDescr: String = "Null"
  }

  trait PrimitiveExprT extends ExprT {
    val expr: Expr
    override def equals(obj: Any): Boolean = obj.getClass == this.getClass
  }

  //  object PrimitiveExprT {
  //    def unapply(exprt: PrimitiveExprT): Option[AST.PrimitiveExpr] = Some(exprt.expr)
  //
  //    def apply(expr: PrimitiveExpr): PrimitiveExprT = expr match {
  //      case Bool(_) => BoolT(expr)
  //      case Num(_) => NumT(expr)
  //      case Char(_) => CharT(expr)
  //      case Str(_) => StrT(expr)
  //    }
  //  }

  case class BoolT(expr: Expr) extends PrimitiveExprT {
    override val toString = "'boolean'"
    lazy val scalaTypeDescr: String = "Boolean"
  }

  case class NumT(expr: Expr) extends PrimitiveExprT {
    override val toString = "'number'"
    lazy val scalaTypeDescr: String = "Double"
  }

  case class CharT(expr: Expr) extends PrimitiveExprT {
    override val toString = "'char'"
    lazy val scalaTypeDescr: String = "Character"
  }

  case class TextT(expr: Expr) extends PrimitiveExprT {
    override val toString = "'text'"
    lazy val scalaTypeDescr: String = "String"
  }

  //  case class ListT(elements_type: ExprT) extends ExprT {
  //    override def toString: String = "'list of " + elements_type + "'"
  //  }

  case class AnyT(expr: Expr) extends PrimitiveExprT {
    override val toString: String = f"'any'"
    override lazy val scalaTypeDescr: String = "Any"
  }

  case class LambdaT(from: ExprT, to: ExprT) extends ExprT {
    override val toString: String = f"{$from → $to}"
    lazy val scalaTypeDescr: String = f"$from => $to"
  }

  case class PartialOpT(pop: PartialOp) extends ExprT {
    override val toString: String = pop.opsymbol
    lazy val scalaTypeDescr: String = pop.opsymbol
  }

  case class Var(id: Int) extends TypeSystem with ExprT {
    var instance: Option[ExprT] = None
    lazy val name: String = id.toChar.toString
    lazy val scalaTypeDescr: String = instance.get.scalaTypeDescr

    override def toString: String = if (instance.isEmpty) name else instance.get.toString
  }

}
