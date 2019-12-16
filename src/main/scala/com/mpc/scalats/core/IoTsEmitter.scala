package com.mpc.scalats.core

import java.io.PrintStream
import scala.collection.immutable.ListSet
import com.mpc.scalats.configuration.Config

// TODO: Emit Option (space-lift?)
// TODO: Use a template engine (velocity?)
final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  def typeAsValArg(s: String): String = s"_${s}val"

  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit = {
    list(declaration).foreach {
      case decl: InterfaceDeclaration =>
        emitIoTsInterfaceDeclaration(decl, out)
      case UnionDeclaration(name, members, possibilities, superInterface) =>
        emitUnionDeclaration(name, members, possibilities, superInterface, out)
      case SingletonDeclaration(name, members, superInterface) =>
        emitSingletonDeclaration(name, members, superInterface, out)
      case _ => ()
    }
  }

  def emitIoTsInterfaceDeclaration(
                                decl: InterfaceDeclaration,
                                out: PrintStream): Unit = {

    val InterfaceDeclaration(name, fields, typeParams, _) = decl

    val typeVal = objectName(name);

    // Class definition
    if (typeParams.isEmpty) {
      out.println(s"export const ${typeVal} = t.type({")
    } else {
      val params = list(typeParams).map(p => s"${p} extends t.Mixed").mkString("<", ", ", ">")
      val args = list(typeParams).map(p => s"${typeAsValArg(p)}: $p").mkString("(", ", ", ")")
      out.println(s"export const ${typeVal} = ${params}${args} => t.type({")
    }

    list(fields).foreach { v =>
      out.println(s"${indent}${v.name}: ${getTypeIoTsString(v.typeRef)},")
    }

    out.println("});")
    if (typeParams.isEmpty) {
      out.println(s"export type ${name} = t.TypeOf<typeof ${typeVal}>;")
    }
    out.println()
  }

  private def emitInterfaceDeclaration(
                                        name: String,
                                        members: ListSet[Member],
                                        superInterface: Option[InterfaceDeclaration],
                                        out: PrintStream): Unit = {
    if (config.emitInterfaces) {
      out.print(s"export interface ${interfaceName(name)}")

      superInterface.foreach { iface =>
        out.print(s" extends ${iface.name}")
      }

      out.println(" {")

      // Abstract fields - common to all the subtypes
      emitMembers(members, true, out)

      out.println("}")
      out.println()
    }
  }

  private def emitUnionDeclaration(
                                    name: String,
                                    members: ListSet[Member],
                                    possibilities: ListSet[CustomTypeRef],
                                    superInterface: Option[InterfaceDeclaration],
                                    out: PrintStream): Unit = {

    // Namespace and union type
    out.println(s"""export type ${name}U = ${list(possibilities).map(p => interfaceName(p.name)).mkString(" | ")};""")
    out.println()
    emitInterfaceDeclaration(name, members, superInterface, out)
  }

  private def emitSingletonDeclaration(
                                        name: String,
                                        members: ListSet[Member],
                                        superInterface: Option[InterfaceDeclaration],
                                        out: PrintStream): Unit = {
    emitInterfaceDeclaration(name, members, superInterface, out)

    out.println(s"export const ${objectName(name)}: ${interfaceName(name)} = {")

    emitMembers(members, false, out)

    out.println("};")
    out.println()
  }

  def emitMembers(members: ListSet[Member], interfaceContext: Boolean, out: PrintStream): Unit = {
    members.foreach(m =>
      out.println(
        m.value.fold(s"${indent}${m.name.trim}: ${getTypeRefString(m.typeRef)};")
        (v => s"${indent}${m.name.trim}: ${getTypeWrappedVal(v, m.typeRef, interfaceContext)},"))
    )
  }

  def getTypeIoTsString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "t.number"
    case BooleanRef => "t.boolean"
    case StringRef => "t.string"
    case DateRef | DateTimeRef => "DateFromISOString"
    case ArrayRef(innerType) => s"t.array(${getTypeIoTsString(innerType)})"
    case CustomTypeRef(name, params) if params.isEmpty => objectName(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${objectName(name)}${params.map(getTypeIoTsString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getTypeIoTsString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getTypeIoTsString(keyType)}, ${getTypeIoTsString(valueType)})"
    case NullRef => "t.null"
    case UndefinedRef => "t.undefined"
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef, interfaceContext: Boolean): String = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s""""${value}""""
    case DateRef | DateTimeRef => s""""${value}""""
    case ArrayRef(_) => s"[${value}]"
    case CustomTypeRef(name, params) if params.isEmpty => if (interfaceContext) interfaceName(name) else objectName(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${if (interfaceContext) interfaceName(name) else objectName(name)}${params.map(getTypeIoTsString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getTypeIoTsString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getTypeIoTsString(keyType)}, ${getTypeIoTsString(valueType)})"
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }

}
