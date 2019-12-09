package com.mpc.scalats.core

import java.io.PrintStream
import scala.collection.immutable.ListSet
import com.mpc.scalats.configuration.Config

// TODO: Emit Option (space-lift?)
// TODO: Use a template engine (velocity?)
final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  def lowercaseFirstChar(s: String) = Character.toLowerCase(s.charAt(0)) + s.substring(1)

  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit = {
    out.println("""import * as t from "io-ts";""")
    out.println()

    list(declaration).foreach {
      case decl: InterfaceDeclaration => emitInterfaceDeclaration(decl, out)
      case _ => ()
    }
  }

  def emitInterfaceDeclaration(
                                decl: InterfaceDeclaration,
                                out: PrintStream): Unit = {

    val InterfaceDeclaration(name, fields, _, _) = decl

    val lcName = lowercaseFirstChar(name);

    // Class definition
    out.println(s"export const ${lcName} = t.type({")

    list(fields).foreach { v =>
      out.println(s"${indent}${v.name}: ${getTypeRefString(v.typeRef)},")
    }

    out.println("});")
    out.println(s"export type ${name} = t.TypeOf<typeof ${lcName}>;")
    out.println()
  }

  def getTypeRefString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "t.number"
    case BooleanRef => "t.boolean"
    case StringRef => "t.string"
    case DateRef | DateTimeRef => "Date"
    case ArrayRef(innerType) => s"t.array(${getTypeRefString(innerType)})"
    case CustomTypeRef(name, params) if params.isEmpty => name
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"$name<${params.map(getTypeRefString).mkString(", ")}>"
    case UnknownTypeRef(typeName) => typeName
    case SimpleTypeRef(param) => param
    case UnionType(possibilities) => s"t.union(${possibilities.map(getTypeRefString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getTypeRefString(keyType)}, ${getTypeRefString(valueType)})"
    case NullRef => "t.null"
    case UndefinedRef => "t.undefined"
  }

}
