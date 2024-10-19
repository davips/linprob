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

import inference.Types.*
import parsing.AST.*

import scala.util.parsing.combinator.PackratParsers

trait Native extends PackratParsers {

  //   def expandScala(typedIdents: List[NamedIdent], code: Text, typStr: String): Lambda = {
  //     val scalaExpr = Scala(typedIdents, code)
  //     var newbody = Sequence(List(scalaExpr))
  //     var res: ExprT = str2type(typStr, scalaExpr)
  //     for (arg <- typedIdents.tail.reverse) {
  //       val la = Lambda(arg, newbody)
  //       la.t = Some(LambdaT(arg.t.get, res))
  //       newbody = Sequence(List(la))
  //       res = la.t.get
  //     }
  //     val la = Lambda(typedIdents.head, newbody)
  //     la.t = Some(LambdaT(typedIdents.head.t.get, res))
  //     la
  //   }

  def buildTypedIdent(id: NamedIdent, typStr: String): NamedIdent = {
    val exprT = str2type(typStr, id)
    id.t = Some(exprT)
    id
  }

  def str2type(typStr: String, id: Expr): PrimitiveExprT = typStr match {
    case "b" => BoolT(id)
    case "c" => CharT(id)
    case "t" => TextT(id)
    case "n" => NumT(id)
  }
}
