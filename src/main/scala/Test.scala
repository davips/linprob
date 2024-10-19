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
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharSequenceReader

object Test extends App with JavaTokenParsers with PackratParsers {

  type P[+T] = PackratParser[T]
  private val program = rep1sep("me" | "m", ";") ~ (";" ~> not("m") ~> "me") ^^ {
    case l ~ r => l :+ r
  }

  val txt2 = "m ;me; me; m; me"
  val chars = new CharSequenceReader(txt2)
  lazy val r: ParseResult[List[String]] = parseAll(phrase(program), new PackratReader(chars))
  println(r)
}
