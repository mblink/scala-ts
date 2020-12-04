package com.mpc.scalats.configuration

import java.io.PrintStream

case class Config(
  emitCodecs: Boolean = true,
  emitInterfaces: Boolean = true,
  fieldNaming: FieldNaming = FieldNaming.Identity,
  outputStream: Option[PrintStream] = None,
  prependIPrefix: Boolean = false,
  tsNamingConvention: Boolean = true,
  typescriptIndent: String = "  ",
  tsImports: TsImports = TsImports()
)

case class TsImports(
  fptsEither: String = "fp-ts/lib/Either",
  fptsThese: String = "fp-ts/lib/These",
  fptsPipe: (String, String) = ("pipe", "fp-ts/lib/Option"),
  iots: String = "io-ts",
  iotsDateTime: (String, String) = ("DateFromISOString", "io-ts-types/lib/DateFromISOString"),
  iotsNonEmptyArray: (String, String) = ("nonEmptyArray", "io-ts-types/lib/nonEmptyArray"),
  iotsNumberFromString: (String, String) = ("NumberFromString", "io-ts-types/lib/NumberFromString"),
  iotsEither: (String, String) = ("either", "io-ts-types/lib/either"),
  iotsOption: (String, String) = ("optionFromNullable", "io-ts-types/lib/optionFromNullable"),
  iotsLocalDate: Option[(String, String)] = None,
  iotsThese: Option[(String, String)] = None
)
