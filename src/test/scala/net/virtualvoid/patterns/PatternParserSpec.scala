package net.virtualvoid.patterns

import org.specs2.mutable.Specification
import Patterns._
import net.virtualvoid.patterns.PatternParser.richMatch
import net.virtualvoid.patterns.Patterns.Unapply1

object PatternParserSpec extends Specification {
  case class Person(name: String)

  "PatternParser" should {
    "AnyValue" in {
      richMatch[Any, String] { case _ => "" }.pattern must_=== AnyValue
    }
    "Constant" in {
      richMatch[Any, String] { case 38 => "" }.pattern must_=== Constant(38)

      val X = 42 // closed-over constant values are inlined
      richMatch[Any, String] { case X => "" }.pattern must_=== Constant(42)
    }
    "TypeCheck" in {
      richMatch[Any, String] { case _: String => "" }.pattern must_=== TypeCheck[String]
    }
    "Unapply0" in {
      object Test {
        def unapply(x: Any): Boolean = x != 42
      }
      val Unapply0(f) = richMatch[Any, String] { case Test() => "" }.pattern
      f(0) must_=== true
      f("test") must_=== true
      f(42) must_=== false
    }
    "Unapply1" in {
      "case class" in {
        val Unapply1(f, AnyValue) = richMatch[Person, String] { case Person(_) => "" }.pattern
        f(Person("test")) must_=== Some("test")
      }
      "extractor" in {
        object Test2 {
          def unapply(x: Any): Option[String] = if (x == 42) None else Some(x.toString)
        }
        val Unapply1(f, AnyValue) = richMatch[Any, String] { case Test2(_) => "" }.pattern
        f(0) must_=== Some("0")
        f("test") must_=== Some("test")
        f(42) must_=== None
      }
    }
    "Bind" in {
      richMatch[Any, String] { case x => "" }.pattern must_=== Bind("x", AnyValue)
      richMatch[Any, String] { case str: String => str }.pattern must_=== Bind("str", TypeCheck[String])
      // Seems to be optimized, as this pattern cannot be observed from the macro, the binding has been optimized away
      // richMatch[Any, String] { case dreiundzwanzisch @ 23 => (dreiundzwanzisch + 12).toString }.pattern must_=== Bind("dreiundzwanzisch", Constant(23))
    }
    "guard" in {
      // check for hygiene, "args" is used internally
      richMatch[Person, String] { case Person(name) if { val args = 123; name startsWith "Otto" } => "" }

      val Guard(Closure(Seq("name"), f), Unapply1(g, Bind("name", AnyValue))) = richMatch[Person, String] { case Person(name) if name startsWith "Otto" => "" }.pattern

      g(Person("test")) must_=== Some("test")
      f(Seq("blub")) must_=== false
      f(Seq("Ottobert")) must_=== true

      val Guard(Closure(Seq("x"), f2), Bind("x", AnyValue)) = richMatch[Int, String] { case x if x > 58 => "" }.pattern

      f2(Seq(12)) must_=== false
      f2(Seq(60)) must_=== true
    }
  }
}
