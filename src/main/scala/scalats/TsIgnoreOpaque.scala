package scalats

import scala.quoted.{Expr, FromExpr, Quotes}

/** Used as an argument to [[scalats.TsParser]] to optionally ignore certain `opaque types` */
final class TsIgnoreOpaque(private val types: Set[String]) extends AnyVal {
  final def contains(s: String): Boolean = types.contains(s)
}

object TsIgnoreOpaque {
  given fromExpr(using Quotes): FromExpr[TsIgnoreOpaque] =
    new FromExpr[TsIgnoreOpaque] {
      def unapply(x: Expr[TsIgnoreOpaque])(using Quotes) = x match {
        case '{ TsIgnoreOpaque(${ Expr(types) }) } => Some(TsIgnoreOpaque(types))
        case _ => None
      }
    }
}
