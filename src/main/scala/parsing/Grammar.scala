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

import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Grammar extends RegexParsers with ImplicitConversions with JavaTokenParsers with Native with Lamb {
  type P[+T] = PackratParser[T]
  private lazy val program = sequence(false) | sequence(true)
  private lazy val sequence = (iargs: Boolean) => rep1sep(if (iargs) iexpr else prettyexpr, separator) ^^ Sequence
  private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"
  private lazy val prettyexpr: P[Expr] = math(false) | expr

  //   private lazy val expr: P[Expr] = "(" ~> sequence(false) <~ ")" | scala | appl | assign | (lambda | ilambda) | term(false) | "{" ~> infixops <~ "}"
  //   private lazy val iexpr: P[Expr] = scala | iassign | lambda | math(true) | term(true) | "{" ~> infixops <~ "}"
  private lazy val expr: P[Expr] = "[" ~> sequence(false) <~ "]" | "(" ~> sequence(false) <~ ")" | assignfun | appl | assign | (lambda | ilambda) | term(false) | "{" ~> infixops <~ "}"
  private lazy val iexpr: P[Expr] = iassign | lambda | math(true) | term(true) | "{" ~> infixops <~ "}"

  private lazy val assignfun: P[Expr] = ((signature <~ "=") ~ prettyexpr) ^^ ((name: NamedIdent, args: List[NamedIdent], exp: Expr) => Assign(name, expandLambda(args, Sequence(List(exp)))))
  private lazy val signature = identifier ~ ("(" ~> rep1(identifier) <~ ")")
  private lazy val assign: P[Expr] = (identifier <~ "=") ~ prettyexpr ^^ Assign
  private lazy val iassign: P[Expr] = (identifier <~ "←") ~ (assign | lambda | math(true) | term(true)) ^^ Assign
  private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence(false) <~ "}") ^^ expandLambda
  private lazy val ilambda = ("{" ~> sequence(true) <~ "}") ^^ iexpandLambda
  //   private lazy val scala = ("{" ~> rep(typedIdent) ~ (str <~ ":") ~ (argType <~ "}")) ^^ expandScala
  private lazy val typedIdent = (identifier <~ ":") ~ argType ^^ buildTypedIdent
  private lazy val argType = "b" | "c" | "t" | "n"
  private lazy val identifier = not("_") ~> ident ^^ NamedIdent
  // infixops serve pra algo ainda?
  private lazy val infixops = ("=" | "=/=" | ">=" | "<=" | ">" | "<" | "+" | "-" | "*" | "/" | "^") ^^ oplamb
  //  private lazy val oplamb = (op: String) => Lambda(AnonIdent(), Sequence(List(Lambda(AnonIdent(), Sequence(List(PartialOp(op)))))))
  private lazy val oplamb = (op: String) => PartialOp(op)
  private lazy val anonidentifier = """_[1-9]+""".r ^^ (idx => AnonIdentN(idx.tail.toInt)) | "_" ^^^ AnonIdent()
  private lazy val math = (iargs: Boolean) => equality(iargs)
  // mudar igual (e todos)? pra (a = b)? → Condition(Assign(a, b))
  // esse não precisa, porque os outros são sempre expressões: (a > b)? → Condition(GreaterThan(a, b))
  //inviável se aninhar, mas dificilmente se aninha '='
  // outra solução é 'a=b' retornar true se forem iguais e false caso contrário/assingment
  //  pra desambiguizar, o valor de assign não pode ser usado numa expressão, nem ser fim de seq (valor de retorno de função)
  // ou o proprio IF pode resolver se acabar em assign, e operadores booleanos também se virem-se operando com assign
  // ainda outra forma é que se a variável à esquerda já existir, assign é tratado como comparação
  // algumas dessas soluções podem não funcionar porque poderiam dependenr de solução após o type checker
  private lazy val equality = (iargs: Boolean) => chainl1(
    sum(iargs), curry("==") | curry("/=") | curry(">=") | curry("<=") | curry(">") | curry("<")
  )
  private lazy val sum = (iargs: Boolean) => chainl1(product(iargs), curry("+") | curry("-"))
  private lazy val product = (iargs: Boolean) => chainl1(power(iargs), curry("*") | curry("/"))
  private lazy val power = (iargs: Boolean) => chainl1(if (iargs) iexpr else expr, curry("^"))
  private lazy val curry = (op: String) => op ^^^ ((a: Expr, b: Expr) => Appl(Appl(oplamb(op), a), b))

  private lazy val term = (iargs: Boolean) => {
    if (iargs) ("[" ~> rep1sep(iexpr, separator) <~ "]" | "(" ~> rep1sep(iexpr, separator) <~ ")") ^^ Sequence | anonidentifier | identifier // inverti anon com ident
    else ("[" ~> rep1sep(prettyexpr, separator) <~ "]" | "(" ~> rep1sep(prettyexpr, separator) <~ ")") ^^ Sequence | identifier
  } | literal | func
  private lazy val literal = num | str | bool
  private lazy val num = floatingPointNumber ^^ (n => Num(n.toDouble))
  private lazy val str = stringLiteral ^^ (str => Text(str.tail.dropRight(1)))
  private lazy val bool: P[Expr] = "↓".r ^^^ Bool(false) | "↑".r ^^^ Bool(true)
  // private lazy val bool = "↓".r ^^^ Bool(false) | "↑".r ^^^ Bool(true)

  private lazy val fargs = "(" ~> rep1sep(expr | func, ",") <~ ")" ^^ Sequence
  private lazy val appl: P[Expr] = (appl ~ fargs | func ~ fargs) ^^ Appl
  private lazy val func: P[Expr] = lambda | ilambda | identifier | "{" ~> infixops <~ "}" | "#" ^^^ Id()

  def parse(txt: String): ParseResult[Sequence] = parseAll(phrase(program), new PackratReader(new CharSequenceReader(txt)))
}
