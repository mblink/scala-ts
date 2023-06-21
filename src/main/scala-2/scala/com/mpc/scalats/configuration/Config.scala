package com.mpc.scalats
package configuration

import java.io.PrintStream
import scala.reflect.runtime.universe.Type

case class Config(
  emitCodecs: Boolean = true,
  emitInterfaces: Boolean = true,
  fieldNaming: FieldNaming = FieldNaming.Identity,
  outputStream: Option[PrintStream] = None,
  prependIPrefix: Boolean = false,
  tsNamingConvention: Boolean = true,
  typescriptIndent: String = "  ",
  tsImports: TsImports = TsImports(),
  getOrdInstance: PartialFunction[core.TypeScriptModel.TypeRef, core.TsImports.With[String]] = PartialFunction.empty,
  scalaTagTypeName: Option[String] = None,
  excludeType: Type => Boolean = _ => false,
)

case class TsImports(
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
