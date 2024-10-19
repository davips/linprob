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

import parsing.AST.*

trait Lamb {
  def expandLambda(args: List[NamedIdent], body: Sequence): Lambda = {
    var newbody = body
    for (arg <- args.tail.reverse) {
      newbody = Sequence(List(Lambda(arg, newbody)))
    }
    Lambda(args.head, newbody)
  }

  def iexpandLambda(body: Sequence): Lambda = {
    def minmax(expr: Expr): Option[(Int, Int)] = {
      val idxs = traverse(expr)
      if (idxs.isEmpty) None
      else Some(idxs.min, idxs.max)
    }

    def traverse(expr: Expr): List[Int] = expr.nested.toList.flatMap {
      case AnonIdent() => List(-1)
      case AnonIdentN(idx) => List(idx)
      case innerExpr => traverse(innerExpr)
    }

    minmax(body) match {
      case None => println("Missing at least one annonymous identifier inside " + body); sys.exit()
      case Some(mn -> mx) =>
        val (firstArg, newbody) = if (mn == -1) {
          if (mx != -1) {
            println("Mixed anonymous identifiers! _ cannot be user together with _1, _2, etc.")
          }
          AnonIdent() -> body
        } else {
          var newbody = body
          for (arg <- (mx to 2 by -1).map(AnonIdentN)) {
            newbody = Sequence(List(Lambda(arg, newbody)))
          }
          AnonIdentN(1) -> newbody
        }

        Lambda(firstArg, newbody)
    }
  }
} 
