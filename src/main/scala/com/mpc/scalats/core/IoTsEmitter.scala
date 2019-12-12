package com.mpc.scalats.core

import java.io.PrintStream
import scala.collection.immutable.ListSet
import com.mpc.scalats.configuration.Config

// TODO: Emit Option (space-lift?)
// TODO: Use a template engine (velocity?)
final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  def typeAsVal(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
  def typeAsValArg(s: String): String = s"_${s}val"

  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit = {
    list(declaration).foreach {
      case decl: InterfaceDeclaration => emitInterfaceDeclaration(decl, out)
      case UnionDeclaration(name, fields, possibilities, superInterface) =>
        emitUnionDeclaration(name, fields, possibilities, superInterface, out)
      case SingletonDeclaration(name, members, superInterface) =>
        emitSingletonDeclaration(name, members, superInterface, out)
      case _ => ()
    }
  }

  def emitInterfaceDeclaration(
                                decl: InterfaceDeclaration,
                                out: PrintStream): Unit = {

    val InterfaceDeclaration(name, fields, typeParams, _) = decl

    val typeVal = typeAsVal(name);

    // Class definition
    if (typeParams.isEmpty) {
      out.println(s"export const ${typeVal} = t.type({")
    } else {
      val params = list(typeParams).map(p => s"${p} extends t.Mixed").mkString("<", ", ", ">")
      val args = list(typeParams).map(p => s"${typeAsValArg(p)}: $p").mkString("(", ", ", ")")
      out.println(s"export const ${typeVal} = ${params}${args} => t.type({")
    }

    list(fields).foreach { v =>
      out.println(s"${indent}${v.name}: ${getTypeRefString(v.typeRef)},")
    }

    out.println("});")
    if (typeParams.isEmpty) {
      out.println(s"export type ${name} = t.TypeOf<typeof ${typeVal}>;")
    }
    out.println()
  }

  private def emitUnionDeclaration(
                                    name: String,
                                    fields: ListSet[Member],
                                    possibilities: ListSet[CustomTypeRef],
                                    superInterface: Option[InterfaceDeclaration],
                                    out: PrintStream): Unit = {

    // Namespace and union type
    out.println(s"""export type $name = ${list(possibilities).map(_.name).mkString(" | ")};""")
    out.println()
//    val discriminatorName = "_type"
//    val children = list(possibilities)

    // Union interface
    if (config.emitInterfaces) {
      out.print(s"export interface ${if (config.prependIPrefix) s"I${name}" else name}")

      superInterface.foreach { iface =>
        out.print(s" extends ${iface.name}")
      }

      out.println(" {")

      // Abstract fields - common to all the subtypes
      list(fields).foreach { member =>
        out.println(s"${indent}${member.name}: ${getTypeRefString(member.typeRef)};")
      }

      out.println("}")
    }
  }

  private def emitSingletonDeclaration(
                                        name: String,
                                        members: ListSet[Member],
                                        superInterface: Option[InterfaceDeclaration],
                                        out: PrintStream): Unit = {

    // Class definition
    out.print(s"export const $name")

    superInterface.foreach { i =>
      out.print(s": ${i.name}")
    }

    out.println(" = {")

    members.foreach(m => println(
      m.value.fold(s"${indent}${m.name}: ${getTypeRefString(m.typeRef)};")
                  (v => s"${indent}${m.name}= ${getTypeWrappedVal(v, m.typeRef)};")))

    out.println("}")
  }

  def getTypeRefString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "t.number"
    case BooleanRef => "t.boolean"
    case StringRef => "t.string"
    case DateRef | DateTimeRef => "DateFromISOString"
    case ArrayRef(innerType) => s"t.array(${getTypeRefString(innerType)})"
    case CustomTypeRef(name, params) if params.isEmpty => typeAsVal(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${typeAsVal(name)}${params.map(getTypeRefString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(_) => "t.unknown"
    case SimpleTypeRef(param) => typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getTypeRefString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getTypeRefString(keyType)}, ${getTypeRefString(valueType)})"
    case NullRef => "t.null"
    case UndefinedRef => "t.undefined"
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef): String = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s""""${value}""""
    case DateRef | DateTimeRef => s""""${value}""""
    case ArrayRef(_) => s"[${value}]"
    case CustomTypeRef(name, params) if params.isEmpty => typeAsVal(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${typeAsVal(name)}${params.map(getTypeRefString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(_) => "unknown"
    case SimpleTypeRef(param) => typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getTypeRefString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getTypeRefString(keyType)}, ${getTypeRefString(valueType)})"
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }

}
