/*
 * Copyright 2016 Andy Scott
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fail.sauce.quickroutes.spray

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox
import scala.reflect.macros.Universe
import scala.reflect.internal.util.FreshNameCreator

import org.scalamacros.resetallattrs._

import cats._
import cats.data.Xor
import cats.std.all._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.functor._

/** Quick route macro implementation.
  */
class QuickRouteMacros(val c: whitebox.Context) {

  private object processors extends QuickRouteTreeProcessors {
    val universe: c.universe.type = c.universe
  }

  import c.universe._
  import processors._

  private lazy val methodNamer = new FreshNameCreator("qr_")

  implicit class Tuple2Ops[T1, T2](t: (T1, T2)) {
    def map1[T01](f: T1 ⇒ T01) = t.copy(_1 = f(t._1))
    def map2[T02](f: T2 ⇒ T02) = t.copy(_2 = f(t._2))
  }

  def append(pf: Tree) =
    (
      // experimentally arranging it this way
      // would like to add some more type information so reading through this
      // makes sense
      unwrapTypedTree(pf) >>=
      unwrapAbstractPartialFunctionStats >>=
      unwrapApplyOrElseExpr >>=
      unwrapMatchCases >>= {
        _.filterNot(isGeneratedDefaultCase)
          .map(unwrapScrutineeCase)
          .map(Functor[Res].lift(_.map2(resetAllAttrs)))
          .sequence
      }
    ) fold (abortError, { info ⇒

        val routeDefDefs = info.map {
          case (d, expr) ⇒
            val wrapperName = TermName(methodNamer.newName(d.describePath))
            spawnDefDefWrapper(wrapperName, d.args, expr)
        }

        val res = q"""
        {
          object quickRoutes {
            ..$routeDefDefs
          }
          ${c.prefix.tree}
        }
        """
        println(res)
        res
      })

  def apiUnapply(scrutinee: Tree) =
    unwrapInterpolation(c.macroApplication) fold (abortError, { info ⇒
      val resTpe: Tree = info.args match {
        case Nil        ⇒ tq"Boolean"
        case tpe :: Nil ⇒ tq"Option[${tpe._2}]"
        case tpes       ⇒ tq"Option[${spawnTupleType(tpes.map(_._2))}]"
      }
      q"_QRPlaceholder(${info.interpolator}, ${info.parts}).unapply[$resTpe]($scrutinee)"
    })

  /** Immediately abort the macro expansion with a compiler error */
  private def abortError(error: Error): Nothing = c.abort(error._1, error._2)

  // c.resetAllAttrs is a macro, force it to expand here
  private lazy val resetAllAttrs = c.resetAllAttrs(_: Tree)

}

/** Quick route macro tree processing methods.
  */
sealed trait QuickRouteTreeProcessors {
  val universe: Universe
  import universe._

  /** An error type a position and a message */
  type Error = (Position, String)

  /** Processor result: either an `Error` or `T` */
  type Res[T] = Xor[Error, T]

  /** Convenience to convert `Tree` to `Position` */
  implicit def tree2pos(tree: Tree): Position = tree.pos

  /** Convenience to convert `List[Tree]` to `Position` */
  implicit def trees2pos(trees: List[Tree]): Position = wrappingPos(trees)

  /** Convenience to convert tuples ? → String to Position → String if we can
    * find ? → String
    */
  implicit def tuple2error[T: ({ type λ[α] = α ⇒ Position })#λ](v: (T, String)): Error =
    implicitly[T ⇒ Position].apply(v._1) → v._2

  /** Decoded information about a routes shape. This is derived from
    * the custom string interpolation calls.
    *
    * @param interpolator the name of the interpolator used. e.g. "get", "post", etc.
    * @param parts the string parts of the interpolated string
    * @param args name → type pairings for extracted arguments
    */
  case class DecodedInterpolation(interpolator: String, parts: List[String], args: List[(Name, Tree)]) {
    def describePath: String = parts match {
      case head :: Nil ⇒ head
      case head :: tail ⇒
        head + (args.map(_._1.toString), tail).zipped.map("$" + _ + _).mkString("")
      case Nil ⇒ "unknown_quick_route"
    }
  }

  /** Generates a `Tuple{N}` type with N type parameters `tpes` */
  def spawnTupleType(tpes: List[Tree]) =
    AppliedTypeTree(
      Select(Ident(TermName("scala")), TypeName(s"Tuple${tpes.length}")),
      tpes
    )

  def spawnDefDefWrapper(name: TermName, args: List[(Name, Tree)], expr: Tree): DefDef = {
    val vparams = args.map { arg ⇒
      ValDef(
        Modifiers(Flag.PARAM),
        arg._1.toTermName,
        arg._2,
        EmptyTree
      )
    }
    q"def $name(..$vparams) = $expr"
  }

  def decodeBind(tree: Tree): Res[(Name, Tree)] = tree match {
    case Bind(pname, inner @ Bind(_, Typed(Ident(termNames.WILDCARD), tpt))) ⇒ Xor.right(pname → tpt)
    case Bind(pname, inner @ Typed(Ident(termNames.WILDCARD), tpt)) ⇒ Xor.right(pname → tpt)
    case Bind(pname, inner) ⇒ Xor.right(pname → tq"String")
    case _ ⇒ Xor.left(tree → s"Unable to decode argument for tree $tree.")
  }

  def unwrapInterpolation(tree: Tree): Res[DecodedInterpolation] = tree match {
    case Apply(internal.reificationSupport.SyntacticTypeApplied(Select(Select(Apply(Select(universe0, _), List(Apply(_, parts))), interpolator), TermName("unapply")), _), args) ⇒
      for {
        subpatterns ← Xor.fromOption(
          internal.subpatterns(args.head),
          tree → s"Unable to gather subpatterns for $args": Error
        )
        decodedArgs ← subpatterns.map(decodeBind).sequence
        unwrappedParts ← parts.map(unwrapConstantString).sequence
      } yield {
        DecodedInterpolation(interpolator.toString, unwrappedParts, decodedArgs)
      }
    case _ ⇒
      Xor.left(tree → s"Couldn't parse call prefix tree ${tree}.")
  }

  def unwrapTypedTree(tree: Tree): Res[Tree] = tree match {
    case Typed(expr, _) ⇒ Xor.right(expr)
    case _              ⇒ Xor.left(tree → "Expected typed expression")
  }

  def unwrapAbstractPartialFunctionStats(tree: Tree): Res[List[Tree]] = tree match {
    case Block(
      q"""$mods class $name1 extends ..$_ { ..$stats }""" :: Nil,
      q"new ${ Ident(name2) }"
      ) if name1 == name2 && mods.hasFlag(Flag.SYNTHETIC) ⇒
      Xor.right(stats)
    case _ ⇒ Xor.left(tree → "Unable to unwrap body of generated partial function.")
  }

  def unwrapApplyOrElseExpr(stats: List[Tree]): Res[Tree] = stats.collectFirst {
    case q"$_ def applyOrElse[$_, $_]($_, $_): $tpt = $expr" ⇒ expr
  } match {
    case Some(expr) ⇒ Xor.right(expr)
    case None       ⇒ Xor.left(stats → "Unable to unwrap generated applyOrElse.")
  }

  def unwrapMatchCases(tree: Tree): Res[List[Tree]] = tree match {
    case q"$expr match { case ..$cases }" ⇒
      Xor.right(cases)
    case _ ⇒
      Xor.left(tree → "Unable to unwrap match statements in generated partial function.")
  }

  def isGeneratedDefaultCase(tree: Tree) = tree match {
    // AbstractPartialFunction has an injected default case that we need to ignore
    case cq"""${ Bind(_, Ident(termNames.WILDCARD)) } => default.apply($_)""" ⇒ true
    case _ ⇒ false
  }

  def unwrapConstantString(tree: Tree): Res[String] = tree match {
    case Literal(Constant(v: String)) ⇒ Xor.right(v)
    case _                            ⇒ Xor.left(tree → "Unable to unwrap constant string.")
  }

  def unwrapConstantListString(tree: Tree): Res[List[String]] = tree match {
    case q"scala.collection.immutable.List.apply[..$_](..$vs)" ⇒
      vs.map(unwrapConstantString).sequence
    case _ ⇒
      Xor.left(tree → "Unable to unwrap list of constant strings.")
  }

  def unwrapScrutineeCase(tree: Tree): Res[(DecodedInterpolation, Tree)] = tree match {
    case cq"""${ z @ UnApply(q"_QRPlaceholder.apply($interpolator0, $parts0).unapply[..$huh](..$_)", args) } => $expr""" ⇒
      for {
        interpolator1 ← unwrapConstantString(interpolator0)
        parts1 ← unwrapConstantListString(parts0)
        decodedArgs ← args.map(decodeBind).sequence
      } yield DecodedInterpolation(interpolator1, parts1, decodedArgs) → expr

    case cq"""${ UnApply(q"_QRPlaceholder.apply($interpolator0, $parts0).unapply[..$_](..$_)", args) } if $condition => $expr""" ⇒
      Xor.left(condition → "Conditions are not yet supported in quick route case expressions.")

    case badTree ⇒
      Xor.left(badTree → "Quick route case expressions must follow DSL syntax.")
  }

}

/** This is used internally as a placeholder during macro expansion.
  */
case class _QRPlaceholder(interpolator: String, parts: List[String]) {
  /** No implementation needed. */
  def unapply[T](x: QuickRouteScrutinee.type): T = ???
}
