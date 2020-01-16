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
    declaration.toList.sortBy({
        case _: SingletonDeclaration => -1
        case _: InterfaceDeclaration => 0
        case _: UnionDeclaration => 1
        case _ => -1
      }).foreach {
        case decl: InterfaceDeclaration =>
          emitIoTsInterfaceDeclaration(decl, out)
        case UnionDeclaration(name, members, possibilities, superInterface) =>
          emitUnionDeclaration(name, members, possibilities, superInterface, out)
        case SingletonDeclaration(name, members, superInterface) =>
          emitSingletonDeclaration(name, members, superInterface, out)
        case _ => ()
      }
      out.println()
      out.println()
  }

  def emitIoTsInterfaceDeclaration(
                                decl: InterfaceDeclaration,
                                out: PrintStream): Unit = {

    val InterfaceDeclaration(name, fields, typeParams, _) = decl

    val typeVal = codecName(name);

    // Class definition
    if (typeParams.isEmpty) {
      out.println(s"export const ${typeVal} = t.type({")
    } else {
      val params = list(typeParams).map(p => s"${p} extends t.Mixed").mkString("<", ", ", ">")
      val args = list(typeParams).map(p => s"${typeAsValArg(p)}: $p").mkString("(", ", ", ")")
      out.println(s"export const ${typeVal} = ${params}${args} => t.type({")
    }

    fields.foreach { v =>
      out.println(s"${indent}${v.name}: ${getIoTsTypeString(v.typeRef)},")
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
      emitMembers(members, true, false, out)

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

    def union(nameFn: (String) => String, p: ListSet[CustomTypeRef]) = list(possibilities).map(p => nameFn(p.name))

    out.println(s"""export const ${codecType(name)}U = t.union(${union(codecName, possibilities).mkString("[", ", ", "]")});""")
    out.println(s"""export const all${name} = ${union(objectName, possibilities).mkString("[", ", ", "]")} as const;""")
    out.println(s"""export type ${name}U = t.TypeOf<typeof ${codecType(name)}U>;""")
    out.println()
    emitInterfaceDeclaration(name, members, superInterface, out)

  }

  private def emitSingletonDeclaration(
                                        name: String,
                                        members: ListSet[Member],
                                        superInterface: Option[InterfaceDeclaration],
                                        out: PrintStream): Unit = {

    out.println(s"export const ${codecName(name)} = t.strict({")
    emitMembers(members, false, true, out)
    out.println("});")
    out.println()

    out.println(s"export type ${interfaceName(name)} = t.TypeOf<typeof ${codecName(name)}>;")
    out.println()

    out.println(s"export const ${objectName(name)}: ${interfaceName(name)} = {")
    emitMembers(members, false, false, out)
    out.println("};")
    out.println()
  }

  def emitMembers(members: ListSet[Member], interfaceContext: Boolean, iotsContext: Boolean, out: PrintStream): Unit = {
    members.foreach(m =>
      out.println(
        m.value.fold(s"${indent}${m.name.trim}: ${getTypeRefString(m.typeRef)};")
        (v => s"${indent}${m.name.trim}: ${
          if (iotsContext)
            getIoTsTypeWrappedVal(v, m.typeRef)
          else
            getTypeWrappedVal(v, m.typeRef, interfaceContext)
        },"))
    )
  }

  def getIoTsTypeString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "t.number"
    case BooleanRef => "t.boolean"
    case StringRef => "t.string"
    case DateRef | DateTimeRef => "DateFromISOString"
    case ArrayRef(innerType) => s"t.array(${getIoTsTypeString(innerType)})"
    case NonEmptyArrayRef(innerType) => s"nonEmptyArray(${getIoTsTypeString(innerType)})"
    case CustomTypeRef(name, params) if params.isEmpty => customIoTsTypes(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${customIoTsTypes(name)}${params.map(getIoTsTypeString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getIoTsTypeString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getIoTsTypeString(keyType)}, ${getIoTsTypeString(valueType)})"
    case NullRef => "t.null"
    case UndefinedRef => "t.undefined"
  }

  def getIoTsTypeWrappedVal(value: Any, typeRef: TypeRef): String = typeRef match {
    case NumberRef => s"t.literal(${value.toString})"
    case BooleanRef => s"t.literal(${value.toString})"
    case StringRef => s"t.literal(`${value.toString.trim}`)"
    case DateRef | DateTimeRef => s"t.literal(new Date(`${value.toString.trim}`))"
    case ArrayRef(_) => s"t.literal([${value}])"
    case NonEmptyArrayRef(_) => s"t.literal(nonEmptyArray.of([${value}]))"
    case CustomTypeRef(name, params) if params.isEmpty => codecName(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${codecName(name)}${params.map(getIoTsTypeWrappedVal(value, _)).mkString("(", ", ", ")")}"
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => s"t.strict(${typeAsValArg(param)})"
    case UnionType(possibilities) => s"t.strict(t.union(${possibilities.map(getIoTsTypeWrappedVal(value, _)).mkString("[", ", ", "]")}))"
    case MapType(keyType, valueType) => s"t.strict(t.record(${getIoTsTypeWrappedVal(value, keyType)}, ${getIoTsTypeWrappedVal(value, valueType)}))"
    case NullRef => "t.literal(null)"
    case UndefinedRef => "t.literal(undefined)"
  }

  def customIoTsTypes(name: String): String = name match {
    case "NonEmptyList" => "nonEmptyArray"
    case "optionFromNullable" => "optionFromNullable"
    case _ => codecName(name)
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef, interfaceContext: Boolean): String = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s"`${value.toString.trim}`"
    case DateRef | DateTimeRef => s"`${value.toString.trim}`"
    case ArrayRef(_) => s"[${value}]"
    case NonEmptyArrayRef(_) => s"nonEmptyArray.of([${value}])"
    case CustomTypeRef(name, params) if params.isEmpty => if (interfaceContext) interfaceName(name) else objectName(name)
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"${if (interfaceContext) interfaceName(name) else objectName(name)}${params.map(getIoTsTypeString).mkString("(", ", ", ")")}"
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
    case UnionType(possibilities) => s"t.union(${possibilities.map(getIoTsTypeString).mkString("[", ", ", "]")})"
    case MapType(keyType, valueType) => s"t.record(${getIoTsTypeString(keyType)}, ${getIoTsTypeString(valueType)})"
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }

}
