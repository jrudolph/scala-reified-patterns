package net.virtualvoid.patterns

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object PatternParser {
  /** Extracts the first case from a pattern */
  def richMatch[T, U](func: PartialFunction[T, U]): Patterns.PatternMatchCase[T, U] = macro richMatchImpl[T, U]

  def richMatchImpl[T: ctx.WeakTypeTag, U: ctx.WeakTypeTag](ctx: blackbox.Context)(func: ctx.Expr[PartialFunction[T, U]]): ctx.Expr[Patterns.PatternMatchCase[T, U]] = {
    import ctx._
    import universe._

    val q"{ case $pattern if $guard => $rhs}" = func.tree

    import Patterns._

    def reifyPattern(pattern: Tree): (ctx.Expr[Pattern[_]], Set[Symbol]) = {
      //println(s"Reifying $pattern ${pattern.getClass}")
      pattern match {
        case pq"_" => reify(AnyValue) -> Set.empty
        case x @ pq"$name @ $inner " =>
          val (i, bindings) = reifyPattern(inner)
          reify(Patterns.Bind(literal(name.toString).splice, i.splice)) -> (bindings + x.symbol)
        case pq"_: $tpe" => ctx.Expr(q"Patterns.TypeCheck[$tpe]") -> Set.empty
        case pq"$x()"    => ctx.Expr(q"Patterns.Unapply0(t => $x.unapply(t))") -> Set.empty
        case pq"$x($t1)" =>
          val realPrefix = x match {
            case t: TypeTree => t.original
            case t           => t
          }
          val (inner, bindings) = reifyPattern(t1)
          ctx.Expr(q"Patterns.Unapply1(t => $realPrefix.unapply(t), $inner)") -> bindings
        case pq"${ expr @ (_: Ident | _: Literal) }" => reify(Patterns.Constant(ctx.Expr[Any](expr).splice)) -> Set.empty
      }
    }

    val (p: ctx.Expr[Pattern[T]], bindings) = reifyPattern(pattern)
    val guarded =
      if (guard.isEmpty) p
      else {
        def transformGuardExpr(e: Tree): (Seq[String], ctx.Expr[Seq[Any] => Boolean]) = {
          var idents: Map[Symbol, Int] = Map.empty
          var nextNum = 0
          val argsName = freshName(TermName("args"))

          object ReplaceBindingsTransformer extends Transformer {
            override def transform(tree: ctx.universe.Tree): ctx.universe.Tree = tree match {
              case i: Ident =>
                idents.get(i.symbol) match {
                  case Some(num) => q"$argsName($num).asInstanceOf[${i.tpe}]"
                  case None =>
                    if (bindings.contains(i.symbol)) {
                      val thisNum = nextNum
                      idents += i.symbol -> thisNum
                      nextNum += 1

                      q"$argsName($thisNum).asInstanceOf[${i.tpe}]"
                    } else i
                }
              case _ => super.transform(tree)
            }
          }

          val replaced = ReplaceBindingsTransformer.transform(e)
          val is = idents.toSeq

          val lambda = q"(($argsName: Seq[Any]) => $replaced)"
          (is.map(_._1.name.toString), ctx.Expr(ctx.untypecheck(lambda)))
        }

        val (args, body) = transformGuardExpr(guard)
        val argsExpr = ctx.Expr[Seq[String]](q"Seq(..${args.map(s => q"$s")})")
        reify(Guard[T](Closure(argsExpr.splice, body.splice), p.splice))
      }

    reify {
      PatternMatchCase(guarded.splice, null)
    }
  }
}
