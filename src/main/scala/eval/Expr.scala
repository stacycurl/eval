package eval

import scala.language.higherKinds

import eval.Expr.Definition

import scala.collection.immutable.{Map => SMap}


sealed trait Expr[A] {
  def interpret(interpreter: Interpreter): Expr[A]

  final def ->(rhs: A): (Expr[A], A) = (this, rhs)
}

object Expr {
  implicit class ExprSyntax[A](private val self: Expr[A]) extends AnyVal {
    def |>[B](f: Expr[A => B]): Expr[B] = ApplyFn(self, f)
  }
  
  implicit class ExprFSyntax[A, F[_]](private val self: Expr[F[A]]) {
    def map[B](fn: Expr[A => B])(implicit N: FName[F]): Expr[F[B]] = self |> fn.lift[F]
  }
  
  implicit class ExprFnSyntax[A, B](private val self: Expr[A => B]) extends AnyVal {
    def andThen[C](f: Expr[B => C]): Expr[A => C] = AndThen(self, f)
    
    def lift[F[_]: FName]: Expr[F[A] => F[B]] = Lift[Function, F, A, B](self, liftFExpr[Function, F])
  }

  case class Literal[A](value: A) extends Expr[A] {
    def interpret(interpreter: Interpreter): Expr[A] = this
  }
  
  case class ApplyFn[A, B](value: Expr[A], fn: Expr[A => B]) extends Expr[B] {
    def interpret(interpreter: Interpreter): Expr[B] = (value.interpret(interpreter), fn.interpret(interpreter)) match {
      case (Literal(a), Literal(f)) => Literal(f(a))
      case _ => this
    }
  }
  
  case class AndThen[A, B, C](ab: Expr[A => B], bc: Expr[B => C]) extends Expr[A => C] {
    def interpret(interpreter: Interpreter): Expr[A => C] = (ab.interpret(interpreter), bc.interpret(interpreter)) match {
      case (Literal(lhs), Literal(rhs)) => Literal(lhs andThen rhs)
      case _                            => this
    }
  }
  
  trait ~~>[F[_, _], G[_, _]] {
    def apply[A, B](fa: F[A, B]): G[A, B]
  }
  
  type Definition[A] = (Expr[A], A)
  
  def liftFExpr[Fn[_, _], F[_]: FName]: Expr[Fn ~~> Lambda[(A, B) => F[A] => F[B]]] = 
    Named[Fn ~~> Lambda[(A, B) => F[A] => F[B]]](s"lift-${FName.name[F]}")
  
  case class Lift[Fn[_, _], F[_], A, B](
    fnExpr: Expr[Fn[A, B]], 
    liftExpr: Expr[Fn ~~> Lambda[(A, B) => F[A] => F[B]]]
  ) extends Expr[F[A] => F[B]] {
    def interpret(interpreter: Interpreter): Expr[F[A] => F[B]] = {
      (fnExpr.interpret(interpreter), liftExpr.interpret(interpreter)) match {
        case (Literal(ab), Literal(lift)) => Literal(lift(ab))
        case _ => this
      }
    }
  }
  
  
  case class Named[A](name: String) extends Expr[A] {
    def interpret(interpreter: Interpreter): Expr[A] = interpreter.lookup[A](this)
  }
}

trait Interpreter {
  def interpret[A](expr: Expr[A]): Expr[A] = expr.interpret(this)
  
  def lookup[A](expr: Expr[A]): Expr[A]
}

object Interpreter {
  case class Real(definitions: Definitions) extends Interpreter {
    def lookup[A](expr: Expr[A]): Expr[A] = definitions.lookup(expr)
  }
  
  object Definitions {
    val empty: Definitions = new Definitions(SMap.empty)
    
    def apply(fns: (Expr[_], _)*): Definitions = new Definitions(fns.toMap)
  }
  
  case class Definitions(definitions: SMap[Expr[_], _]) {
    def lookup[A](expr: Expr[A]): Expr[A] = definitions.get(expr) match {
      case Some(value) => Expr.Literal(value.asInstanceOf[A])
      case None        => expr
    }
  
    def :+[A, B](definition: Definition[A]): Definitions = copy(definitions + definition)
  }
}

trait FName[F[_]] {
  def name: String
}

object FName {
  def name[F[_]: FName]: String = of[F].name
  
  def of[F[_]](implicit F: FName[F]): FName[F] = F
  
  implicit val listFName: FName[List] = create[List]("list")
  
  implicit val optionFName: FName[Option] = create[Option]("option")
  
  private def create[F[_]](name0: String): FName[F] = new FName[F] {
    val name: String = name0
  }
}