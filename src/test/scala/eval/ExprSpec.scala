package eval

import scala.language.higherKinds

import org.scalatest.{FreeSpec, Matchers}
import scalaz.{Functor, Monad}
import scalaz.std.list._
import scalaz.std.option._


class ExprSpec extends FreeSpec with Matchers {
  "interpret" in {
    import Expr._
    import Interpreter._

    val greeting  = Name[String]("greeting")

    val reverse   = Name[String => String]("reverse")
    val upperCase = Name[String => String]("upper-case")
    val lowerCase = Name[String => String]("lower-case")

    val tails = Name[String => List[String]]("tails")
    val vowels = Name[String => Option[String]]("vowels")

    val vowelsImpl: String => Option[String] = value => {
      val filtered = value.filter(Set('a', 'e', 'i', 'o', 'u'))

      if (filtered.isEmpty) None else Some(filtered)
    }

    def functorDefinition[F[_]: Functor: F1Name]: Definition[Function ~~> Lambda[(A, B) => F[A] => F[B]]] = {
      Expr.liftFExpr[Function, Lambda[(A, B) => F[A] => F[B]]] -> new (Function ~~> Lambda[(A, B) => F[A] => F[B]]) {
        def apply[A, B](fa: A => B): F[A] => F[B] = Functor[F].lift(fa)
      }
    }

    def monadDefinition[M[_]: Monad: F1Name]: Definition[Lambda[(A, B) => A => M[B]] ~~> Lambda[(A, B) => M[A] => M[B]]] = (
      Expr.liftFExpr[Lambda[(A, B) => A => M[B]], Lambda[(A, B) => M[A] => M[B]]],
      new (Lambda[(A, B) => A => M[B]] ~~> Lambda[(A, B) => M[A] => M[B]]) {
        def apply[A, B](fa: A => M[B]): M[A] => M[B] = Monad[M].bind(_)(fa)
      }
    )

    val interpreter = Interpreter.Real(
      Definitions(
        greeting   := "Ola !",
        reverse    := (_.reverse),
        upperCase  := (_.toUpperCase),
        vowels     := vowelsImpl,
        tails      := (_.tails.toList),
        functorDefinition[Option],
        functorDefinition[List],
        monadDefinition[Option],
        monadDefinition[List],
//        Expr.liftFExpr[Lambda[(A, B) => A => List[B]], Lambda[(A, B) => List[A] => List[B]]] -> new (
//          Lambda[(A, B) => A => List[B]] ~~> Lambda[(A, B) => List[A] => List[B]]
//        ) {
//          def apply[A, B](fa: A => List[B]): List[A] => List[B] = _.flatMap(fa)
//        }
//        Expr.liftFExpr[List] -> new Expr.LiftFunction[List] {
//          def apply[A, B](fa: A => B): List[A] => List[B] = _.map(fa)
//        },
//        Expr.liftFExpr[Option] -> new Expr.LiftFunction[Option] {
//          def apply[A, B](fa: A => B): Option[A] => Option[B] = _.map(fa)
//        }
      )
    )

    assert(Literal("olleh") === interpreter.interpret(Literal("hello") |> reverse))
    assert(Literal("HELLO") === interpreter.interpret(Literal("hello") |> upperCase))
    assert(Literal("OLLEH") === interpreter.interpret(Literal("hello") |> upperCase.andThen(reverse)))
    assert((Literal("hello") |> upperCase.andThen(lowerCase)) === interpreter.interpret(Literal("hello") |> upperCase.andThen(lowerCase)))

    assert(Literal("! alO") === interpreter.interpret(greeting |> reverse))

    assert(Literal(List("olleh")) === interpreter.interpret(Literal(List("hello")).map(reverse)))
    assert(Literal(Some("HELLO")) === interpreter.interpret(Literal(Option("hello")).map(upperCase)))

    assert(Literal(List("hello", "ello", "llo", "lo", "o", "")) === interpreter.interpret(Literal(List("hello")).flatMap(tails)))
    assert(Literal(Some("eo")) === interpreter.interpret(Literal(Option("hello")).flatMap(vowels)))
  }
}