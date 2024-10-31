// Copyright (c) 2021. Davi Pereira dos Santos
// This file is part of the tupi project.
// Please respect the license - more about this in the section (*) below.
//
// tupi is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// tupi is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with tupi.  If not, see <http://www.gnu.org/licenses/>.
//
// (*) Removing authorship by any means, e.g. by distribution of derived
// works or verbatim, obfuscated, compiled or rewritten versions of any
// part of this work is illegal and it is unethical regarding the effort and
// time spent here.
//

package parsing

import parsing.AST.*

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Grammar extends RegexParsers with ImplicitConversions with JavaTokenParsers with Native with Lamb {
    type P[+T] = PackratParser[T]
    private lazy val program = sequence
    private lazy val sequence = rep1sep(prettyexpr, separator) ^^ { case (lst: List[Expr]) =>
        val newlst = ListBuffer[Expr]()
        lst.foreach {
            case Assigns(as) => newlst.addAll(as)
            case x => newlst.addOne(x)
        }
        Sequence(newlst.toList)
    }
    private lazy val prettyexpr: P[Expr] = math | expr | ("outra coisa" ~> ident ^^ NamedIdent.apply)
    private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"

    private lazy val math = comparisonPrecedence
    private lazy val comparisonPrecedence = chainl1(sumLikePrecendence, curry("<") | curry("<=") | curry("≤") | curry("=") | curry(">=") | curry("≥") | curry(">") | curry("≠"))
    private lazy val sumLikePrecendence = chainl1(productLikePrecedence, curry("+") | curry("-"))
    private lazy val productLikePrecedence = chainl1(powerLikePrecedence, curry("·") | curry("×") | curry("*") | curry("/"))
    private lazy val powerLikePrecedence = chainl1(expr, curry("^") | curry("**"))
    private lazy val curry = (op: String) => op ^^^ ((a: Expr, b: Expr) => Appl(Appl(oplamb(op), a), b))
    private lazy val oplamb = (op: String) => op match {
        case "<" | "≤" | "=" | "≥" | ">" | "≠" | "+" | "-" | "·" | "/" | "^" | "∧" | "∨" | "⊻" => PartialOp(op)
        case "<=" => PartialOp("≤")
        case ">=" => PartialOp("≥")
        case "!=" | "/=" | "=/=" => PartialOp("≠")
        case "*" | "×" => PartialOp("·")
        case "**" => PartialOp("^")
        case "&" => PartialOp("∧")
        case "|" => PartialOp("∨")
        case "^^" => PartialOp("⊻")
        case x => println(f"Operador desconhecido: $x")
            sys.exit()
    }

    private lazy val expr: P[Expr] = cases | assign | assigns | term //| parentesized | assignfun | appl | assigns | assign | term(false) | negative | sqroot | ("other things" ~> ident ^^ NamedIdent.apply) //| "{" ~> infixops <~ "}"
    private lazy val cases: P[Appl] = (("{" ~> rep1sep(singleCase, separator)) ~ (";" ~> otherCase)) ^^ { case (cs: List[Case]) ~ (c: Case) =>
        var a = Appl(PartialOp("!"), c)
        cs.foreach { case Case(value, condition) =>
            a = Appl(condition, Appl(a, value))
        }
        a
    }
    private lazy val singleCase: P[Case] = (expr <~ ",") ~ ("se" ~> math) ^^ Case.apply
    private lazy val otherCase: P[Case] = expr <~ ", cc." ^^ ((e: Expr) => Case(e, Bool(true)))

    private lazy val assign = "seja" ~> singleAssign
    private lazy val singleAssign = (identifier <~ "=") ~ prettyexpr ^^ Assign.apply
    private lazy val identifier = not("_") ~> ident ^^ NamedIdent.apply
    private lazy val assigns = ("sejam" ~> rep1sep(singleAssign, ",")) ^^ Assigns.apply


    private lazy val term = literal | identifier //| seqqq  //| func
    private lazy val literal = num | str | bool
    //    private lazy val seqqq = ("[" ~> rep1sep(prettyexpr, separator) <~ "]" | "(" ~> rep1sep(prettyexpr, separator) <~ ")") ^^ ((lst: List[Expr]) => Sequence(lst))
    private lazy val num = floatingPointNumber ^^ (n => Num(n.toDouble))
    private lazy val str = stringLiteral ^^ (str => Text(str.tail.dropRight(1)))
    private lazy val bool: P[Bool] = ("⊥".r | "⊭".r | "↓".r | "F".r) ^^^ Bool(false) | ("⊤".r | "⊨".r | "↑".r | "V".r) ^^^ Bool(true)


    //
    //    //    private lazy val expr: P[Expr] = parentesized | assignfun | appl | assigns | assign | (lambda | ilambda) | term(false) | negative | sqroot | ("other things" ~> ident ^^ NamedIdent) //| "{" ~> infixops <~ "}"
    //    private lazy val parentesized: P[Expr] = "[" ~> sequence(false) <~ "]" | "(" ~> sequence(false) <~ ")"
    //
    //    private lazy val negative: P[Expr] = "-" ~> (identifier | parentesized) ^^ (x => Appl(Appl(PartialOp("·"), Num(-1)), x))
    //    private lazy val sqroot: P[Expr] = "√" ~> (identifier | parentesized) ^^ (x => Appl(Appl(PartialOp("√"), Num(2)), x))
    //    private lazy val negation: P[Expr] = "¬" ~> (identifier | parentesized) ^^ (x => Appl(PartialOp("¬"), x))
    //    //    private lazy val show: P[Expr] = identifier <~ "?  " ^^ Show
    //
    //    //    private lazy val iexpr: P[Expr] = iassign | lambda | math(true) | term(true) //| "{" ~> infixops <~ "}"
    //    private lazy val iexpr: P[Expr] = iassign | math(true) | term(true) //| "{" ~> infixops <~ "}"
    //
    //    private lazy val assignfun: P[Expr] = ((signature <~ "=") ~ prettyexpr) ^^ ((name: NamedIdent, args: List[NamedIdent], exp: Expr) => Assign(name, expandLambda(args, Sequence(List(exp)))))
    //    private lazy val signature = "seja" ~> (identifier ~ ("(" ~> rep1(identifier) <~ ")"))
    //
    //
    //    //    private lazy val iassign: P[Expr] = (identifier <~ "←") ~ (assigns | assign | lambda | math(true) | term(true)) ^^ Assign
    //    private lazy val iassign: P[Expr] = (identifier <~ "←") ~ (assigns | assign | math(true) | term(true)) ^^ Assign
    //
    //    //    private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence(false) <~ "}") ^^ expandLambda
    //    //    private lazy val ilambda = ("{" ~> sequence(true) <~ "}") ^^ iexpandLambda // infix lambda?
    //    //   private lazy val scala = ("{" ~> rep(typedIdent) ~ (str <~ ":") ~ (argType <~ "}")) ^^ expandScala
    //    private lazy val typedIdent = (identifier <~ ":") ~ argType ^^ buildTypedIdent
    //    private lazy val argType = "b" | "c" | "t" | "n"
    //    //    private lazy val infixops = ("=" | "=/=" | ">=" | "<=" | ">" | "<" | "+" | "-" | "*" | "/" | "^") ^^ oplamb
    //
    //    // private lazy val bool = "↓".r ^^^ Bool(false) | "↑".r ^^^ Bool(true)
    //    //    ☑✅✓✔⍻🗸🗹🮱☐☑☒✗✘⮽🗳🗴🗵🗶🗷🗹
    //    //
    //
    //    private lazy val fargs = "(" ~> rep1sep(expr | func, ",") <~ ")" ^^ ((lst: List[Expr]) => Sequence(lst))
    //    private lazy val appl: P[Expr] = (appl ~ fargs | func ~ fargs) ^^ Appl
    //    //    private lazy val func: P[Expr] = lambda | ilambda | identifier /* "{" ~> infixops <~ "}" | */ ^^^ Id()
    //    private lazy val func: P[Expr] = identifier /* "{" ~> infixops <~ "}" | */ ^^^ Id()

    def parse(txt: String): ParseResult[Sequence] = parseAll(phrase(program), new PackratReader(new CharSequenceReader(txt)))
}
