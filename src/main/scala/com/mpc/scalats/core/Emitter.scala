package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel.{ArrayRef, BooleanRef, CustomTypeRef, DateRef, DateTimeRef, Declaration, MapType, NullRef, NumberRef, SimpleTypeRef, StringRef, TypeRef, UndefinedRef, UnionType, UnknownTypeRef}
import java.io.PrintStream
import scala.collection.immutable.ListSet

trait Emitter {
  val config: Config
  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit

  val indent: String = config.typescriptIndent
  def indent(num: Int): String = config.typescriptIndent * num
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

  def getTypeRefString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "number"
    case BooleanRef => "boolean"
    case StringRef => "string"
    case DateRef | DateTimeRef => "Date"
    case ArrayRef(innerType) => s"${getTypeRefString(innerType)}[]"
    case CustomTypeRef(name, params) if params.isEmpty => name
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"$name<${params.map(getTypeRefString).mkString(", ")}>"
    case UnknownTypeRef(typeName) => typeName
    case SimpleTypeRef(param) => param

    case UnionType(possibilities) =>
      possibilities.map(getTypeRefString).mkString("(", " | ", ")")

    case MapType(keyType, valueType) => s"{ [key: ${getTypeRefString(keyType)}]: ${getTypeRefString(valueType)} }"

    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
