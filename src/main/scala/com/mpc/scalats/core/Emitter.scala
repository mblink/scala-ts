package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel._
import java.io.PrintStream
import scala.collection.immutable.ListSet

trait Emitter {
  val config: Config
  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit

  val indent: String = config.typescriptIndent
  def indent(num: Int): String = config.typescriptIndent * num
  def pluralize(s: String): String = if (s.endsWith("s")) s else s"${s}s"
  def typeParameters(params: ListSet[String]): String =
    if (params.isEmpty) "" else params.mkString("<", ", ", ">")
  def interfaceName(name: String): String = {
    val n = if (config.prependIPrefix) {
      s"I${name}"
    } else {
      name
    }

    if (config.tsNamingConvention) n.head.toString.toUpperCase ++ n.tail else n
  }
  def objectName(name: String): String = {
    if (config.tsNamingConvention) {
      name.head.toString.toLowerCase ++ name.tail
    } else {
      name
    }
  }
  def codecName(name: String): String = s"${objectName(name)}C"
  def codecType(name: String): String = s"${interfaceName(name)}C"

  def getTypeRefString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "number"
    case BooleanRef => "boolean"
    case StringRef => "string"
    case DateRef => "LocalDate"
    case DateTimeRef => "Date"
    case ArrayRef(innerType) => s"${getTypeRefString(innerType)}[]"
    case NonEmptyArrayRef(innerType) => s"NonEmptyArray<${getTypeRefString(innerType)}>"
    case CustomTypeRef(name, params) =>
      if (params.isEmpty) name
      else s"$name<${params.map(getTypeRefString).mkString(", ")}>"
    case UnknownTypeRef(typeName) => typeName
    case SimpleTypeRef(param) => param

    case UnionType(possibilities) =>
      possibilities.map(getTypeRefString).mkString("(", " | ", ")")

    case EitherType(lT, rT) => s"E.Either<${getTypeRefString(lT)}, ${getTypeRefString(rT)}>"

    case TheseType(lT, rT) => s"Th.These<${getTypeRefString(lT)}, ${getTypeRefString(rT)}>"

    case MapType(keyType, valueType) => s"{ [key: ${getTypeRefString(keyType)}]: ${getTypeRefString(valueType)} }"

    case TupleType(types) => types.map(getTypeRefString).mkString("[", ", ", "]")

    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
