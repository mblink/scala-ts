package sts

import cats.syntax.reducible.*
import scala.quoted.*
import scala.util.chaining.*

trait TypeParser {
  implicit val ctx: Quotes

  import ctx.reflect.*

  protected enum T {
    case TypeParam[A]()(using val a: Type[A])
    case Json
    case Number
    case BigNumber
    case Boolean
    case String
    case LocalDate
    case DateTime
    case Eval[A]()(using val a: Type[A])
    case List[A](toList: Expr[Any => scala.List[A]])(using val a: Type[A])
    case Set[A]()(using val a: Type[A])
    case NonEmptyList[A](toNel: Expr[Any => cats.data.NonEmptyList[A]])(using val a: Type[A])
    case Option[A]()(using val a: Type[A])
    case Either[L, R](toEither: Expr[Any => scala.Either[L, R]])(using val l: Type[L], val r: Type[R])
    case Ior[L, R](toIor: Expr[Any => cats.data.Ior[L, R]])(using val l: Type[L], val r: Type[R])
    case Map[K, V]()(using val k: Type[K], val v: Type[V])
    case EmptyTuple
    case TupleRec[H, T <: Tuple]()(using val h: Type[H], val t: Type[T])
    case TupleN[T]()(using val t: Type[T])
    case Unknown[A]()(using val a: Type[A])
  }

  protected final def parseType(typeRepr: TypeRepr): T =
    typeRepr.asType match {
      case '[TypeParam[a]] => T.TypeParam[a]()
      case '[io.circe.Json] => T.Json
      case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => T.Number
      case '[BigDecimal] => T.BigNumber
      case '[Boolean] => T.Boolean
      case '[String] => T.String
      case '[java.time.LocalDate] | '[org.joda.time.LocalDate] => T.LocalDate
      case '[java.time.LocalDateTime] | '[java.time.ZonedDateTime] | '[java.time.Instant] | '[org.joda.time.DateTime] => T.DateTime
      case '[cats.Eval[a]] => T.Eval[a]()
      case '[cats.data.Chain[a]] => T.List[a]('{ (_: Any).asInstanceOf[cats.data.Chain[a]].toList })
      case '[List[a]] => T.List[a]('{ (_: Any).asInstanceOf[List[a]] })
      case '[Vector[a]] => T.List[a]('{ (_: Any).asInstanceOf[Vector[a]].toList })
      case '[Seq[a]] => T.List[a]('{ (_: Any).asInstanceOf[Seq[a]].toList })
      case '[Set[a]] => T.Set[a]()
      case '[collection.immutable.SortedSet[a]] => T.Set[a]()
      case '[cats.data.NonEmptyChain[a]] => T.NonEmptyList[a]('{ (_: Any).asInstanceOf[cats.data.NonEmptyChain[a]].toNonEmptyList })
      case '[cats.data.NonEmptyList[a]] => T.NonEmptyList[a]('{ (_: Any).asInstanceOf[cats.data.NonEmptyList[a]] })
      case '[cats.data.NonEmptyVector[a]] => T.NonEmptyList[a]('{ (_: Any).asInstanceOf[cats.data.NonEmptyVector[a]].toNonEmptyList })
      case '[scalaz.NonEmptyList[a]] => T.NonEmptyList[a]('{ (_: Any).asInstanceOf[scalaz.NonEmptyList[a]].pipe(l => cats.data.NonEmptyList(l.head, l.tail.toList)) })
      case '[Option[a]] => T.Option[a]()
      case '[Either[l, r]] => T.Either[l, r]('{ (_: Any).asInstanceOf[Either[l, r]] })
      case '[scalaz.\/[l, r]] => T.Either[l, r]('{ (_: Any).asInstanceOf[scalaz.\/[l, r]].toEither })
      case '[cats.data.Ior[l, r]] => T.Ior[l, r]('{ (_: Any).asInstanceOf[cats.data.Ior[l, r]] })
      case '[scalaz.\&/[l, r]] => T.Ior[l, r]('{ (_: Any).asInstanceOf[scalaz.\&/[l, r]].fold(cats.data.Ior.Left(_), cats.data.Ior.Right(_), cats.data.Ior.Both(_, _)) })
      case '[Map[k, v]] => T.Map[k, v]()
      case '[EmptyTuple] => T.EmptyTuple
      case '[*:[h, t]] => T.TupleRec[h, t]()
      case '[t] if typeRepr <:< TypeRepr.of[Tuple] => T.TupleN[t]()
      case '[t] => T.Unknown[t]()
    }
}
