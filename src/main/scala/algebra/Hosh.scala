// Copyright (c) 2021. Davi Pereira dos Santos
// This file is part of the tupi project.
// Please respect the license - more about this in the section (*) below.
//
// tupi is free software: you can redistribute it and/or pify
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

// package algebra
//
// import ky.korins.blake3.Blake3
//
// case class Hosh(cells: Array[BigInt]) {
//   lazy val alph: Array[Char] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-.".toCharArray
//   lazy val n: BigInt = cells(5) +
//     cells(4) * Hosh.p +
//     cells(3) * Hosh.p.pow(2) +
//     cells(2) * Hosh.p.pow(3) +
//     cells(1) * Hosh.p.pow(4) +
//     cells(0) * Hosh.p.pow(5)
//   lazy val id: String = {
//     val txt = Array.fill(40)('0')
//     var divmod: (BigInt, BigInt) = n -> 0
//     for (i <- 0 to 39) {
//       divmod = divmod._1 /% 64
//       txt(i) = alph.charAt(divmod._2.toInt)
//     }
//     txt.mkString
//   }
//
//   def *(that: Hosh): Hosh = Hosh(Array(
//     (cells(0) + that.cells(0)) % Hosh.p,
//     (cells(1) + that.cells(1)) % Hosh.p,
//     (cells(2) + that.cells(2) + cells(3) * that.cells(0)) % Hosh.p,
//     (cells(3) + that.cells(3)) % Hosh.p,
//     (cells(4) + that.cells(4) + cells(1) * that.cells(3)) % Hosh.p,
//     (cells(5) + that.cells(5) + cells(1) * that.cells(2) + cells(4) * that.cells(0)) % Hosh.p
//   ))
// }
//
// object Hosh {
//   lazy val p: BigInt = 1099511627689L
//   lazy val order: BigInt = p.pow(6)
//   def apply(blob: Array[Byte]): Hosh = {
//     val cells: Array[BigInt] = Array.fill(6)(0)
//     val num: BigInt = Blake3.bigInt(blob, 240) % order
//     var divmod: (BigInt, BigInt) = num -> 0
//     for (i <- 5 to 0 by -1) {
//       divmod = divmod._1 /% p
//       cells(i) = divmod._2
//     }
//     Hosh(cells)
//   }
// }
