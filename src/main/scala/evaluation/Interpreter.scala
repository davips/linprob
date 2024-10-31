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

package evaluation

import parsing.AST.*
import runtime.LMap

import scala.math.pow

object Interpreter {
    //    private val output = ListBuffer[String]()
    def ev(e: Expr, m: LMap[Expr]): (Expr, LMap[Expr]) = {
        //        println(f"ev:   $e")
        val e2 -> m2 = e match {
            case Ident(name) => m(name) -> m
            case Assign(x, y) =>
                if (m.get(x.name).isDefined) {
                    println(x.name + " already assigned!")
                    sys.exit()
                }
                Empty() -> m.put(x.name, y)
            case Sequence(x :: List()) => ev(x, m)
            case Sequence((a@Assign(_, _)) :: xs) => ev(Sequence(xs), ev(a, m)._2)
            case Sequence(x :: xs) => ev(Sequence(xs), m) // TODO: tail call optimization
            case Appl(Id(), Ident(name)) => Text("##hosh##") -> m
            case Appl(Id(), e) => Text("##hosh##") -> m
            //       case Appl(Id(), Ident(name)) =>  Text(m(name).hosh.get.id) -> m
            //       case Appl(Id(), e) =>  Text(e.hosh.get.id) -> m
            case Appl(f, x) => ev(f, m)._1 -> ev(x, m)._1 match {
                case Closure(PartialOp(opsymbol), ctx) -> (value: Num) => OpTo(opsymbol, value) -> m
                case Closure(OpTo(opsymbol, Num(a)), ctx) -> Num(b) =>
                    val r = opsymbol match {
                        case "+" => a + b
                        case "-" => a - b
                        case "·" => a * b
                        case "/" => a / b
                        case "^" => pow(a, b)
                        case "=" => a == b
                        case "≠" => a != b
                        case "<" => a < b
                        case "≤" => a <= b
                        case "≥" => a >= b
                        case ">" => a > b
                        case "√" => pow(b, 1 / a)
                        case x => println(f"Operador binário numérico desconhecido: $x"); sys.exit()
                    }
                    PrimitiveExpr(r) -> m
                case Closure(OpTo(opsymbol, Bool(b)), ctx) -> bev =>
                    val r = opsymbol match {
                        case "¬" => !b
                        case "?" => b
                        case b => println(f"Operador unário lógico desconhecido: $x"); sys.exit()
                    }
                    PrimitiveExpr(r) -> m
                case Closure(OpTo(opsymbol, Bool(a)), ctx) -> Bool(b) =>
                    val r = opsymbol match {
                        case "∧" => a && b
                        case "∨" => a || b
                        case x => println(f"Operador binário lógico desconhecido: $x"); sys.exit()
                    }
                    PrimitiveExpr(r) -> m
                case Closure(Lambda(param, Sequence(List(body))), ctx) -> xev => ev(body, ctx.put(param.name, xev))
            }
            case p: PrimitiveExpr => p -> m
            //       case s@Scala(params, _) => s.func(params.map(x => m(x.name))) -> m
            case la: (Lambda | PartialOp | OpTo) => Closure(la, m) -> m // We need a closure here, since Lambda can be returned as a value to be applied later
            //            case Show(e) =>
            //                val r = ev(e, m)
            //                output += (f"Resposta: ${r._1}")
            //                r
        }
        if (e2.isInstanceOf[PrimitiveExpr]) (e2, m2) else ev(e2, m2)
    }

    def eval(e: Expr): Any = {
        val re = ev(e, LMap())
        re._1.asInstanceOf[PrimitiveExpr].value
        //        output.foreach{row =>
        //            println(row)
        //        }
    }
}
