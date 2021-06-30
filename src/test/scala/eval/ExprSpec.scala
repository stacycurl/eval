package eval

import scala.language.higherKinds

import org.scalatest.{FreeSpec, Matchers}
import scalaz.Functor
import scalaz.std.list._
import scalaz.std.option._


class ExprSpec extends FreeSpec with Matchers {
  "interpret" in {
    import Expr._
    import Interpreter._
    
    val greeting  = Named[String]("greeting")
    
    val reverse   = Named[String => String]("reverse")
    val upperCase = Named[String => String]("upper-case")
    val lowerCase = Named[String => String]("lower-case")
    
    def functorExpr[F[_]: Functor: FName]: (Expr[LiftFunction[F]], LiftFunction[F]) = {
      Expr.liftFExpr[F] -> new Expr.LiftFunction[F] {
        def apply[A, B](fa: A => B): F[A] => F[B] = Functor[F].lift(fa)
      }
    }
    
    val interpreter = Interpreter.Real(
      Lookup(
        greeting   -> "Ola !", 
        reverse    -> (_.reverse),
        upperCase  -> (_.toUpperCase),
        functorExpr[Option],
        functorExpr[List],
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
  }
}