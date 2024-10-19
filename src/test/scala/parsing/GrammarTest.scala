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

package parsing

// import inference.Types.NumT
// import parsing.AST._
//
// class GrammarTest extends org.scalatest.funsuite.AnyFunSuite {
//
//   List(
//     "↑" -> Bool(true),
//     "↓" -> Bool(false),
//     "0" -> Num(0),
//     "x" -> NamedIdent("x"),
//     "#x" -> NamedIdent("x"),
//     "(x;y)" -> Sequence(List(NamedIdent("x"), NamedIdent("y"))),
//     "{_}" -> Lambda(AnonIdent(), Sequence(List(AnonIdent()))),
//     "{_1; _2}" -> Lambda(AnonIdentN(1), Sequence(List(Lambda(AnonIdentN(2), Sequence(List(AnonIdentN(1), AnonIdentN(2))))))),
//     "{a:n \"a+a\":n}" -> Lambda(NamedIdent("a"), Sequence(List(Scala(List(NamedIdent("a")), Text("a+a")))))
//   ) foreach {
//     case (text, ast) => test(text) {
//       assert(Grammar.parse(text).get.items.head == ast)
//     }
//   }
//
//   //  test("Invoking head on an empty Set should produce NoSuchElementException") {
//   //    assertThrows[NoSuchElementException] {
//   //      Set.empty.head
//   //    }
//   //  }
// }
