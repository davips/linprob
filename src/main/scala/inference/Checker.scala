/*
 * http://dysphoria.net/code/hindley-milner/HindleyMilner.scala
 * Andrew Forrest
 *
 * Implementation of basic polymorphic type-checking for a simple language.
 * Based heavily on Nikita Borisov’s Perl implementation at
 * http://web.archive.org/web/20050420002559/www.cs.berkeley.edu/~nikitab/courses/cs263/hm.html
 * which in turn is based on the paper by Luca Cardelli at
 * http://lucacardelli.name/Papers/BasicTypechecking.pdf
 *
 * If you run it with "scala HindleyMilner.scala" it will attempt to report the types
 * for a few example expressions. (It uses UTF-8 for output, so you may need to set your
 * terminal accordingly.)
 *
 */

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

package inference

import inference.Types.*
import parsing.AST.*

import scala.annotation.tailrec

class Undefined(msg: String) extends Exception(msg)

class TypeError(msg: String) extends Exception(msg)

class ParseTypeError(msg: String) extends Exception(msg)

class TypeSystem {

    type Env = Map[String, ExprT]
    private var varid: Int = 944

    private def newVar: Var = {
        varid += 1
        Var(varid)
    }

    @tailrec
    private def analyse_sequence(items: List[(Expr, Option[ExprT])], env: Env, nongen: Set[Var], verbosity: Int): ExprT = {
        var newenv = env
        val res = for ((expr, opt) <- items) yield {
            if (opt.isDefined) expr -> opt
            else try {
                val (typ, env2) = analyse(expr, newenv, nongen, verbosity)
                newenv = env2
                expr -> Some(typ)
            } catch {
                case p: Undefined => expr -> None
            }
        }
        if (res.last._2.isEmpty) {
            if (res.flatMap(_._2).size > items.flatMap(_._2).size)
                analyse_sequence(res, newenv, nongen, verbosity)
            else {
                analyse(res.last._1, newenv, nongen, verbosity)
                throw new Undefined("Exception not thrown for " + res.last._1)
            }
        } else res.last._2.get
    }

    private def handleAssign(id: NamedIdent, e: Expr, env: Env, nongen: Set[Var], verbosity: Int): Env = {
        var newenv = env
        try {
            val (etype, _) = analyse(e, env, nongen, verbosity)
            newenv += (id.name -> etype)
        } catch { //loop?
            case p: Undefined if p.getMessage == id.name =>
                println(p.getMessage)
                println("Loop detected!")
                val newtype = newVar
                newenv += (id.name -> newtype)
                val (etype, _) = analyse(e, newenv, nongen + newtype, verbosity)
                unify(newtype, etype)
                analyse(e, newenv, nongen, verbosity)
            // This case should be available only in case of error.
            // Otherwise, the type checker will stop before knowing symbols that are defined after usage.
            case t: Undefined if verbosity == 1 => print(s"Não consegui resolver '${t.getMessage}'\t")
        }
        newenv
    }
    def analyse(ast: Expr, env: Env, verbosity: Int = 1): ExprT = analyse(ast, env, Set.empty, verbosity)._1

    private def analyse(ast: Expr, env: Env, nongen: Set[Var], verbosity: Int): (ExprT, Env) = {
        //    println("AST: " + ast)
        var newenv = env
        if (ast.t.isDefined) (ast.t.get, env) else {
            val t = ast match {
                //        case s: Scala =>
                //          s.t.get
                case Sequence(items) => analyse_sequence(items.zip(LazyList.continually(None)), env, nongen, verbosity)
                case Ident(name) => gettype(name, env, nongen)
                case id@Id() =>
                    LambdaT(AnyT(id), TextT(id)) // TODO está correto passar id como expr?
                case Appl(fn, arg) =>
                    val (funtype, _) = analyse(fn, env, nongen, verbosity)
                    val (argtype, _) = analyse(arg, env, nongen, verbosity)
                    val resulttype = newVar
                    unify(LambdaT(argtype, resulttype), funtype) // TODO: verify if we need to pass some expr here for LambdaT
                    resulttype
                case pop: PartialOp =>
                    LambdaT(NumT(pop), LambdaT(NumT(pop), NumT(pop))) //PartialOpT(pop)
                case Lambda(arg, body) =>
                    val argtype = newVar
                    val (resulttype, _) = analyse(body, env + (arg.name -> argtype), nongen + argtype, verbosity)
                    LambdaT(argtype, resulttype)
                case Assign(id, e) =>
                    newenv = handleAssign(id, e, newenv, nongen, verbosity)
                    EmptyT
                case n: Num => NumT(n)
                case t: Text => TextT(t)
                //                case c: Conditional => BoolT(c)
                //                case Show(e) => analyse(e, env, nongen, debug)._1
            }
            if (verbosity >= 2) print(t.toString + ":\t")
            if (verbosity >= 1) println("%-41s".format(ast.toString).grouped(62).mkString(" ...\n  ... "))
            (t, newenv)
        }
    }

    private def gettype(name: String, env: Env, nongen: Set[Var]): ExprT = {
        if (env.contains(name))
            fresh(env(name), nongen)
        else
            throw new Undefined(name)
    }

    private def fresh(t: ExprT, nongen: Set[Var]): ExprT = {
        import inference.Types.PrimitiveExprT

        import scala.collection.mutable
        val mappings = new mutable.HashMap[Var, Var]

        def freshrec(tp: ExprT): ExprT = {
            prune(tp) match {
                case v: Var => if (isgeneric(v, nongen)) mappings.getOrElseUpdate(v, newVar) else v
                case LambdaT(from, to) => LambdaT(freshrec(from), freshrec(to))
                case EmptyT => EmptyT
                case pet: (PrimitiveExprT | PartialOpT) => pet //need PartialOpT here?
            }
        }

        freshrec(t)
    }


    private def unify(t1: ExprT, t2: ExprT, reverse: Boolean = false): Unit = {
        val type1 = prune(t1)
        val type2 = prune(t2)
        //    println("ty1: " + type1 + " ty2:" + type2)
        (type1, type2) match {
            case (_, AnyT(_)) | (AnyT(_), _) =>
            case (a: Var, b) => if (a != b) {
                if (occursintype(a, b))
                    throw new TypeError("recursive unification")
                a.instance = Some(b)
            }
            case (a, b: Var) => unify(b, a, reverse = true)
            case (LambdaT(froma, toa), LambdaT(fromb, tob)) =>
                unify(froma, fromb)
                unify(toa, tob)
            case (la@LambdaT(froma, body), func2: PrimitiveExprT) =>
                val exp = func2.expr.t.get
                if (reverse) throw new TypeError("Esperado:\t" + la + "\nEncontrado:  \t" + exp)
                else throw new TypeError("Esperado:\t" + exp + "\nEncontrado:  \t" + la)
            case (a, b: PrimitiveExprT) if a != b =>
                throw new TypeError(f"Tipos não combinam: '${b.expr}' deveria ser '$b' e não '$a'.")
            case (a: PrimitiveExprT, b) if a != b =>
                throw new TypeError(f"Tipos não combinam: '${a.expr}' deveria ser '$a' e não '$b'.")
            case (a, b) if a != b => throw new TypeError("Tipos não combinam: " + a + " ≠ " + b)
            case (a, b) =>
        }
    }


    // Returns the currently defining instance of t.
    // As a side effect, collapses the list of type instances.
    private def prune(t: ExprT): ExprT = t match {
        case v: Var if v.instance.isDefined =>
            val inst = prune(v.instance.get)
            v.instance = Some(inst)
            inst
        case _ => t
    }

    // Note: must be called with v 'pre-pruned'
    private def isgeneric(v: Var, nongen: Set[Var]): Boolean = !occursin(v, nongen)

    // Note: must be called with v 'pre-pruned'
    private def occursintype(v: Var, type2: ExprT): Boolean = {
        prune(type2) match {
            case `v` => true
            //      case Oper(name, args) => occursin(v, args) // acredito que o escopo e o parser já resolvam
            case _ => false
        }
    }

    private def occursin(t: Var, list: Iterable[ExprT]): Boolean =
        list exists (t2 => occursintype(t, t2))

    //  val checkDigits: Regex = "^(\\d+)$".r  //  def isIntegerLiteral(name: String) = checkDigits.findFirstIn(name).isDefined
}

object HM extends TypeSystem {
    def check(ast: Expr, env: Env = Map.empty, verbosity: Int = 1): Boolean = {
        try {
            analyse(ast, env, verbosity = verbosity) // TODO flag verbose log
        } catch {
            case t: Undefined =>
                if (verbosity >= 1) println(s"Não consegui resolver '${t.getMessage}'.")
                return false
            case t: TypeError =>
                print(t.getMessage)
                return false
        }
        //    println()
        true
    }
}
