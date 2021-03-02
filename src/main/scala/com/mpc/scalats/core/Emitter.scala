package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel._
import scala.collection.immutable.ListSet

trait Emitter extends TsImports.HelperSyntax {
  val config: Config
  lazy val imports: TsImports.available = TsImports.available(config)

  type Lines = TsImports.With[List[String]]

  def line(): Lines = (imports.empty, List(""))
  def line(s: String): Lines = (imports.empty, List(s))
  def line(t: TsImports.With[String]): Lines = (t._1, List(t._2))

  implicit def listStringToLines(ls: List[String]): Lines = (imports.empty, ls)
  implicit def stringToTsImportsWithString(s: String): TsImports.With[String] = (imports.empty, s)

  def emit(declaration: ListSet[Declaration])(implicit ctx: TsImports.Ctx): Lines

  val indent: String = config.typescriptIndent
  def indent(num: Int): String = config.typescriptIndent * num
  def pluralize(s: String): String = if (s.endsWith("s")) s else s"${s}s"
  def typeParameters(params: ListSet[String]): String =
    if (params.isEmpty) "" else params.mkString("<", ", ", ">")

  private val allCapsRx = """^([A-Z0-9_]+)$""".r
  private def naming(name: String, modFirst: Char => Char): String = name match {
    case allCapsRx(n) => n
    case n if config.tsNamingConvention => modFirst(n.head) +: n.tail
    case n => n
  }

  def interfaceName(name: String): String = naming(if (config.prependIPrefix) s"I${name}" else name, _.toUpper)
  def objectName(name: String): String = naming(name, _.toLower)
  def codecName(name: String): String = s"${objectName(name)}C"
  def codecType(name: String): String = s"${interfaceName(name)}C"

  def getTypeRefString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => "number"
    case BooleanRef => "boolean"
    case StringRef => "string"
    case DateRef => imports.iotsLocalDate
    case DateTimeRef => "Date"
    case ArrayRef(innerType) => getTypeRefString(innerType) |+| "[]"
    case NonEmptyArrayRef(innerType) => imports.iotsReadonlyNonEmptyArray.value |+| "<" |+| getTypeRefString(innerType) |+| ">"
    case CustomTypeRef(name, params, scalaType) =>
      if (params.isEmpty) imports.custom(scalaType, name)
      else name |+| params.joinTypeParams(getTypeRefString)
    case UnknownTypeRef(typeName) => typeName
    case SimpleTypeRef(param) => param

    case UnionType(name, possibilities, scalaType) =>
      imports.custom(scalaType, name).orElse(possibilities.joinParens(" | ")(getTypeRefString))

    case EitherType(lT, rT) =>
      imports.fptsEither("Either") |+| List(lT, rT).joinTypeParams(getTypeRefString)

    case TheseType(lT, rT) =>
      imports.fptsThese("These") |+| List(lT, rT).joinTypeParams(getTypeRefString)

    case MapType(keyType, valueType) =>
      "{ [key: " |+| getTypeRefString(keyType) |+| "]: " |+| getTypeRefString(valueType) |+| " }"

    case TupleType(types) =>
      types.joinArray(getTypeRefString)

    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
