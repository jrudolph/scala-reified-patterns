package net.virtualvoid.patterns

import org.specs2.mutable.Specification
import Patterns._

class PatternSpec extends Specification {
  case class Person(name: String)

  "Pattern" should {
    "match correctly" in {
      "underscore" in {
        Patterns.matches("abc", AnyValue) must ===(MatchesNoBindings)
        Patterns.matches(12, AnyValue) must ===(MatchesNoBindings)
        Patterns.matches(null, AnyValue) must ===(MatchesNoBindings)
      }
      "constant" in {
        Patterns.matches(12, Constant(12)) must ===(MatchesNoBindings)
        Patterns.matches(42, Constant(12)) must ===(None)
        Patterns.matches(null, Constant[Any](12)) must ===(None)
        Patterns.matches("12345", Constant[Any](12)) must ===(None)
      }
      "typecheck" in {
        Patterns.matches(12, TypeCheck[java.lang.Integer]) must ===(MatchesNoBindings)
        Patterns.matches("string", TypeCheck[Int]) must ===(None)
        Patterns.matches("string", TypeCheck[String]) must ===(MatchesNoBindings)
        Patterns.matches("string", TypeCheck[AnyRef]) must ===(MatchesNoBindings)
      }
      "binding" in {
        Patterns.matches(12, Bind("num", AnyValue)) must ===(Some(Map("num" -> 12)))
        Patterns.matches(12, Bind("str", TypeCheck[String])) must ===(None)
      }
      "nested binding" in {
        val ExtractCaseClassValueAndField: Pattern[Person] =
          Bind("person", Unapply1(Person.unapply, Bind("name", AnyValue)))
        Patterns.matches(Person("Glubsch"), ExtractCaseClassValueAndField) must ===(Some(Map("person" -> Person("Glubsch"), "name" -> "Glubsch")))
      }
      "unapply1" in {
        val ExtractSomeValue: Pattern[Some[String]] = Unapply1(Some.unapply[String], Bind("val", AnyValue))
        Patterns.matches(Some("test"), ExtractSomeValue) must ===(Some(Map("val" -> "test")))

        // Can't be written because runtime typecheck is missing:
        // Patterns.matches(None, ExtractSomeValue) must ===(None)
      }
      "product1" in {
        val Tuple1ContentIsAnn = Product1Pattern(Constant("Ann"))
        Patterns.matches(Tuple1("Ann"), Tuple1ContentIsAnn) must ===(MatchesNoBindings)
        Patterns.matches(Tuple1("John"), Tuple1ContentIsAnn) must ===(None)
      }
      "product2" in {
        val Tuple2FirstIs0SecondIsString = Product2Pattern(Constant(0), TypeCheck[String])
        Patterns.matches(0 -> "test", Tuple2FirstIs0SecondIsString) must ===(MatchesNoBindings)
        Patterns.matches(0 -> 38, Tuple2FirstIs0SecondIsString) must ===(None)
        Patterns.matches(42 -> "blub", Tuple2FirstIs0SecondIsString) must ===(None)

        val ExtractBoth = Product2Pattern(Bind("one", AnyValue), Bind("two", AnyValue))
        Patterns.matches(23 -> "text", ExtractBoth) must ===(Some(Map("one" -> 23, "two" -> "text")))
      }
      "typechecked unapply1" in {
        val ExtractSomeValue: Pattern[Option[String]] = typeChecked[Option[String], Some[String]](Unapply1(Some.unapply[String], Bind("val", AnyValue)))
        Patterns.matches(Some("test"), ExtractSomeValue) must ===(Some(Map("val" -> "test")))
        Patterns.matches(None, ExtractSomeValue) must ===(None)
      }
      "guard" in {
        val PersonNameStartsWithOtto =
          Guard( // equivalent to `case Person(name) if name startsWith "Otto"
            Closure(Seq("name"), name => name(0).asInstanceOf[String].startsWith("Otto")),
            Unapply1(Person.unapply, Bind("name", AnyValue))
          )

        Patterns.matches(Person("Karl"), PersonNameStartsWithOtto) must ===(None)
        Patterns.matches(Person("Ottofred"), PersonNameStartsWithOtto) must ===(Some(Map("name" -> "Ottofred")))
      }
    }
  }
}
