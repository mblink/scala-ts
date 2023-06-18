package sts

import scala.quoted.*

case class Config(
  tsImports: TsImportsConfig = TsImportsConfig(),
  // getOrdInstance: PartialFunction[core.TypeScriptModel.TypeRef, core.TsImports.With[String]] = PartialFunction.empty,
  scalaTagTypeName: Option[String] = None,
  // excludeType: Type => Boolean = _ => false,
)

object Config {
  given fromExpr: FromExpr[Config] = new FromExpr[Config] {
    def unapply(x: Expr[Config])(using Quotes): Option[Config] = x match {
      case '{ Config(${ Expr(imports) }, ${ Expr(tagTypeName) }) } => Some(Config(imports, tagTypeName))
      case _ => None
    }
  }
}

/*
  given OptionFromExpr[T](using Type[T], FromExpr[T]): FromExpr[Option[T]] with {
    def unapply(x: Expr[Option[T]])(using Quotes) = x match {
      case '{ Option[T](${Expr(y)}) } => Some(Option(y))
      case '{ None } => Some(None)
      case '{ ${Expr(opt)} : Some[T] } => Some(opt)
      case _ => None
    }
  }
*/

case class TsImportsConfig(
  fptsBoolean: String = "fp-ts/lib/boolean",
  fptsDate: String = "fp-ts/lib/Date",
  fptsEither: String = "fp-ts/lib/Either",
  fptsNumber: String = "fp-ts/lib/number",
  fptsOption: String = "fp-ts/lib/Option",
  fptsOrd: String = "fp-ts/lib/Ord",
  fptsReadonlyArray: String = "fp-ts/lib/ReadonlyArray",
  fptsReadonlySet: String = "fp-ts/lib/ReadonlySet",
  fptsString: String = "fp-ts/lib/string",
  fptsThese: String = "fp-ts/lib/These",
  fptsPipe: (String, String) = ("pipe", "fp-ts/lib/function"),
  iots: String = "io-ts",
  iotsDateTime: (String, String) = ("DateFromISOString", "io-ts-types/lib/DateFromISOString"),
  iotsReadonlyNonEmptyArray: (String, String) = ("readonlyNonEmptyArray", "io-ts-types/lib/readonlyNonEmptyArray"),
  iotsReadonlySetFromArray: (String, String) = ("readonlySetFromArray", "io-ts-types/lib/readonlySetFromArray"),
  iotsNumberFromString: (String, String) = ("NumberFromString", "io-ts-types/lib/NumberFromString"),
  iotsOption: (String, String) = ("optionFromNullable", "io-ts-types/lib/optionFromNullable"),
  iotsBigNumber: Option[(String, String)] = None,
  iotsEither: Option[(String, String)] = None,
  iotsLocalDate: Option[(String, String)] = None,
  iotsThese: Option[(String, String)] = None
)

object TsImportsConfig {
  given fromExpr: FromExpr[TsImportsConfig] = new FromExpr[TsImportsConfig] {
    def unapply(x: Expr[TsImportsConfig])(using Quotes): Option[TsImportsConfig] = x match {
      case '{
        TsImportsConfig(
          ${ Expr(fptsBoolean) },
          ${ Expr(fptsDate) },
          ${ Expr(fptsEither) },
          ${ Expr(fptsNumber) },
          ${ Expr(fptsOption) },
          ${ Expr(fptsOrd) },
          ${ Expr(fptsReadonlyArray) },
          ${ Expr(fptsReadonlySet) },
          ${ Expr(fptsString) },
          ${ Expr(fptsThese) },
          ${ Expr(fptsPipe) },
          ${ Expr(iots) },
          ${ Expr(iotsDateTime) },
          ${ Expr(iotsReadonlyNonEmptyArray) },
          ${ Expr(iotsReadonlySetFromArray) },
          ${ Expr(iotsNumberFromString) },
          ${ Expr(iotsOption) },
          ${ Expr(iotsBigNumber) },
          ${ Expr(iotsEither) },
          ${ Expr(iotsLocalDate) },
          ${ Expr(iotsThese) },
        )
      } => Some(TsImportsConfig(
        fptsBoolean,
        fptsDate,
        fptsEither,
        fptsNumber,
        fptsOption,
        fptsOrd,
        fptsReadonlyArray,
        fptsReadonlySet,
        fptsString,
        fptsThese,
        fptsPipe,
        iots,
        iotsDateTime,
        iotsReadonlyNonEmptyArray,
        iotsReadonlySetFromArray,
        iotsNumberFromString,
        iotsOption,
        iotsBigNumber,
        iotsEither,
        iotsLocalDate,
        iotsThese,
      ))
      case _ => None
    }
  }
}
