package eval

import scala.language.higherKinds

import eval.Expr.Definition

import scala.collection.immutable.{Map => SMap}


sealed trait Expr[A] {
  def interpret(interpreter: Interpreter): Expr[A]

  final def :=(rhs: A): (Expr[A], A) = (this, rhs)
}

object Expr {
  implicit class ExprSyntax[A](private val self: Expr[A]) extends AnyVal {
    def |>[B](f: Expr[A => B]): Expr[B] = ApplyFn(self, f)
  }
  
  implicit class ExprFSyntax[A, F[_]](private val self: Expr[F[A]]) {
    def map[B](fn: Expr[A => B])(implicit N: F1Name[F]): Expr[F[B]] = self |> fn.lift[F]
    
    def flatMap[B](fn: Expr[A => F[B]])(implicit N: F1Name[F]): Expr[F[B]] = self |> fn.lift 
  }
  
  implicit class ExprFnSyntax[A, B](private val self: Expr[A => B]) extends AnyVal {
    def andThen[C](f: Expr[B => C]): Expr[A => C] = AndThen(self, f)
    
    def lift[F[_]: F1Name]: Expr[F[A] => F[B]] = Lift[Function, Lambda[(A, B) => F[A] => F[B]], A, B](
      self, 
      liftFExpr[Function, Lambda[(A, B) => F[A] => F[B]]]
    )
  }

  implicit class ExprKleisliSyntax[F[_], A, B](private val self: Expr[A => F[B]]) extends AnyVal {
    def andThen[C](f: Expr[B => F[C]]): Expr[A => F[C]] = >=>(self, f)
    
    def lift(implicit N: F1Name[F]): Expr[F[A] => F[B]] = Lift[Lambda[(A, B) => A => F[B]], Lambda[(A, B) => F[A] => F[B]], A, B](
      self, 
      liftFExpr[Lambda[(A, B) => A => F[B]], Lambda[(A, B) => F[A] => F[B]]]
    )
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
  
  case class >=>[F[_], A, B, C](afb: Expr[A => F[B]], bfc: Expr[B => F[C]]) extends Expr[A => F[C]] {
    def interpret(interpreter: Interpreter): Expr[A => F[C]] = {
      ???
    } 
  }
  
  trait ~~>[F[_, _], G[_, _]] {
    def apply[A, B](fa: F[A, B]): G[A, B]
  }
  
  type Definition[A] = (Expr[A], A)
  
  def liftFExpr[Fn[_, _]: F2Name, Gn[_, _]: F2Name]: Expr[Fn ~~> Gn] = 
    Name[Fn ~~> Gn](s"lift-${F2Name.name[Fn]}-to-${F2Name.name[Gn]}")
  
  case class Lift[Fn[_, _], Gn[_, _], A, B](
    fnExpr: Expr[Fn[A, B]], 
    liftExpr: Expr[Fn ~~> Gn]
  ) extends Expr[Gn[A, B]] {
    def interpret(interpreter: Interpreter): Expr[Gn[A, B]] = {
      (fnExpr.interpret(interpreter), liftExpr.interpret(interpreter)) match {
        case (Literal(ab), Literal(lift)) => Literal(lift(ab))
        case _ => this
      }
    }
  }
  
  case class Name[A](name: String) extends Expr[A] {
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

trait F1Name[F[_]] {
  def name: String
}

object F1Name {
  def name[F[_]: F1Name]: String = of[F].name
  
  def of[F[_]](implicit F: F1Name[F]): F1Name[F] = F
  
  implicit val listFName: F1Name[List] = create[List]("list")
  
  implicit val optionFName: F1Name[Option] = create[Option]("option")
  
  private def create[F[_]](name0: String): F1Name[F] = new F1Name[F] {
    val name: String = name0
  }
}

trait F2Name[F[_, _]] {
  def name: String
}

object F2Name {
  def name[F[_, _]: F2Name]: String = of[F].name
  
  def of[F[_, _]](implicit F: F2Name[F]): F2Name[F] = F
  
  implicit val functionName: F2Name[Function] = create[Function]("function")
  
  implicit def kleisli[F[_]: F1Name]: F2Name[Lambda[(A, B) => A => F[B]]] = 
    create[Lambda[(A, B) => A => F[B]]](s"kleisli-in-${F1Name.name[F]}")
  
  implicit def liftedFunction[F[_]: F1Name]: F2Name[Lambda[(A, B) => F[A] => F[B]]] = 
    create[Lambda[(A, B) => F[A] => F[B]]](s"list-function-to-${F1Name.name[F]}")
  
  private def create[F[_, _]](name0: String): F2Name[F] = new F2Name[F] {
    val name: String = name0
  }
}