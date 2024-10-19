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
package runtime

import scala.collection.mutable

case class LMap[T](m: Map[String, () => T] = Map[String, () => T]()) {
  val cache: mutable.Map[String, Option[T]] = mutable.Map.empty[String, Option[T]]

  def put(k: String, v: => T): LMap[T] = LMap(m + (k -> (() => v)))

  def get(k: String): Option[T] = {
    cache.getOrElse(k, m.get(k).map(_ ()))
  }

  def +(other: LMap[T]): LMap[T] = LMap(m ++ other.m)

  def apply(k: String): T = get(k).get
}

object LMapTest extends App {
  var m: LMap[Int] = LMap[Int]()
  m = m.put("a", 3)
  m = m.put("b", m("a") * 5)
  println(m("b"))
  println(m("b"))
  println(m("a"))
  println(m("a"))
  println(m("a"))
}
