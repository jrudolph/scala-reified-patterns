package net.virtualvoid.patterns

import scala.reflect.ClassTag

object Patterns {
  /**
   * A closure that references bindings with the given names. The function needs to be called with values
   * corresponding to those bindings.
   */
  case class Closure[+T](closedOverVariableNames: Seq[String], func: Seq[Any] ⇒ T)
  case class PatternMatchCase[T, U](pattern: Pattern[T], rhs: Closure[U])

  /** A pattern that can match a value of type T and extract bindings in the process. Corresponds to the lhs of a "case" statement. */
  sealed trait Pattern[-T]

  case class Unapply1[T, U](unapplier: T ⇒ Option[U], inner: Pattern[U]) extends Pattern[T]
  case class Product1Pattern[T1](inner: Pattern[T1]) extends Pattern[Product1[T1]]
  case class Product2Pattern[T1, T2](inner1: Pattern[T1], inner2: Pattern[T2]) extends Pattern[Product2[T1, T2]]
  case class Bind[T](name: String, inner: Pattern[T]) extends Pattern[T]
  case class Alternatives[T](alts: Seq[Pattern[T]]) extends Pattern[T]

  /** A guard similar to one in `case` statements. However, this guard can be used at any level. */
  case class Guard[T](filter: Closure[Boolean], inner: Pattern[T]) extends Pattern[T]

  /** A simple pattern without bindings */
  sealed abstract class SimplePattern[-T](val matches: T => Boolean) extends Pattern[T]
  case class Unapply0[T](unapplier: T ⇒ Boolean) extends SimplePattern[T](unapplier)
  case class TypeCheck[T]()(implicit classTag: ClassTag[T]) extends SimplePattern[Any](classTag.runtimeClass.isInstance)
  case class Constant[T](expected: T) extends SimplePattern[T](expected == _)
  case object AnyValue extends SimplePattern[Any](_ => true)

  def typeChecked[T, U <: T: ClassTag](inner: Pattern[U]): Pattern[T] =
    Unapply1[T, U]({ case u: U => Some(u); case _ => None }, inner)

  val MatchesNoBindings: Option[Map[String, Any]] = Some(Map.empty)

  def runClosure[T](closure: Closure[T], bindings: Map[String, Any]): T =
    closure.func(closure.closedOverVariableNames.map(bindings))

  /** An interpreter for patterns */
  def matches[T](value: T, pattern: Pattern[T]): Option[Map[String, Any]] = pattern match {
    case p: SimplePattern[T]        => if (p.matches(value)) MatchesNoBindings else None

    case Guard(filter, inner)       => matches(value, inner).filter(runClosure(filter, _))
    case Alternatives(alts)         => alts.iterator.map(matches(value, _)).find(_.isDefined).flatten // FIXME: check for binding compatibility or only allow simple patterns
    case Bind(name, inner)          => matches(value, inner).map(_.updated(name, value))
    case Unapply1(unapplier, inner) => unapplier(value).flatMap(matches(_, inner))
    case p: Product1Pattern[_] =>
      def matchProduct1[T](p1: Product1[T], pattern: Product1Pattern[T]): Option[Map[String, Any]] =
        matches(p1._1, pattern.inner)

      matchProduct1(value, p)
    case p: Product2Pattern[_, _] =>
      def matchProduct2[T1, T2](p: Product2[T1, T2], pattern: Product2Pattern[T1, T2]): Option[Map[String, Any]] =
        for {
          t1bindings <- matches(p._1, pattern.inner1)
          t2bindings <- matches(p._2, pattern.inner2)
        } yield t1bindings ++ t2bindings // FIXME: don't allow overlapping bindings

      matchProduct2(value, p)
  }
}
