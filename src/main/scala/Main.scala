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

import evaluation.Interpreter
import inference.HM
import parsing.{AST, Grammar}

import scala.io.{BufferedSource, Source}

object Main extends App {
  val arq: BufferedSource = Source.fromFile("test.tupi")
  val txt: Iterator[String] = arq.getLines().map(_.strip).filter(_.nonEmpty)
  val txt1: String = txt.mkString("\n").takeWhile(_ != '¬').replace("²", "^2").replace("³", "^3")
  //  println(txt1)
  val txt2: String = txt1.replace("\n", ";\n").replace("(;\n", "(\n").replace("{;\n", "{\n").replace(";\n)", "\n)").replace(";\n}", "\n}").replace(":;", ":").
    replace(";;", ";").lines().filter(!_.startsWith("~")).toArray().toList.mkString //.lines().map(_.trim).toArray().toList.mkString("\n").replace("\n}", "ŋ}").replace("{\n", "ŋ{").replace(":\n", "ŋ:").replace("\n", ";\n").replace("ŋ:", ":\n").replace("ŋ{", "{\n").replace("ŋ}", "\n}")
  val txt3: String = if (txt2.endsWith(";")) txt2.dropRight(1) else txt2
  val st: Long = System.currentTimeMillis()
  val parsed: Grammar.ParseResult[AST.Sequence] = Grammar.parse(txt3)
  println(("" + (System.currentTimeMillis() - st)) + "ms")
  println(String.valueOf(parsed) + "\n")
  if (!parsed.successful) sys.exit()
  if (!HM.check(parsed.get)) sys.exit()
  val re: Any = Interpreter.eval(parsed.get)
  println(re)
}
