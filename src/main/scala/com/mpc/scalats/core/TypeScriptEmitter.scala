package com.mpc.scalats.core

import java.io.PrintStream

import scala.collection.immutable.ListSet

import com.mpc.scalats.configuration.{ Config, FieldNaming }

// TODO: Emit Option (space-lift?)
// TODO: Use a template engine (velocity?)
final class TypeScriptEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  // TODO: If for ClassDeclaration or SingletonDeclaration there is values
  // implementing the superInterface, then do not 'implements'
  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit = {
    list(declaration).foreach { d =>
      d match {
        case decl: InterfaceDeclaration =>
          emitInterfaceDeclaration(decl, out)

        case decl: ClassDeclaration =>
          emitClassDeclaration(decl, out)

        case SingletonDeclaration(name, members, superInterface) =>
          emitSingletonDeclaration(name, members, superInterface, out)

        case UnionDeclaration(name, fields, possibilities, superInterface, _) =>
          emitUnionDeclaration(
            name, fields, possibilities, superInterface, out)
      }
    }
  }

  // ---

  private def emitUnionDeclaration(
    name: String,
    fields: ListSet[Member],
    possibilities: ListSet[CustomTypeRef],
    superInterface: Option[InterfaceDeclaration],
    out: PrintStream): Unit = {

    // Namespace and union type
    out.println(s"export namespace $name {")
    out.println(s"""${indent}type Union = ${possibilities.map(_.name) mkString " | "};""")

    if (config.emitCodecs) {
      // TODO: Discriminator naming
      val discriminatorName = "_type"
      val naming: String => String = identity[String](_)
      val children = list(possibilities)

      // Decoder factory: MyClass.fromData({..})
      out.println(s"\n${indent}public static fromData(data: any): ${name} {")
      out.println(s"${indent(2)}switch (data.${discriminatorName}) {")

      children.foreach { sub =>
        val clazz = if (sub.name startsWith "I") sub.name.drop(1) else sub.name

        out.println(s"""${indent(3)}case "${naming(sub.name)}": {""")
        out.println(s"${indent(4)}return ${clazz}.fromData(data);")
        out.println(s"${indent(3)}}")
      }

      out.println(s"${indent(2)}}")
      out.println(s"${indent}}")

      // Encoder
      out.println(s"\n${indent}public static toData(instance: ${name}): any {")

      children.zipWithIndex.foreach {
        case (sub, index) =>
          out.print(s"${indent(2)}")

          if (index > 0) {
            out.print("} else ")
          }

          val clazz =
            if (sub.name startsWith "I") sub.name.drop(1) else sub.name

          out.println(s"if (instance instanceof ${sub.name}) {")
          out.println(s"${indent(3)}const data = ${clazz}.toData(instance);")
          out.println(s"""${indent(3)}data['$discriminatorName'] = "${naming(sub.name)}";""")
          out.println(s"${indent(3)}return data;")
      }

      out.println(s"${indent(2)}}")
      out.println(s"${indent}}")
    }

    out.println("}")

    // Union interface
    out.print(s"\nexport interface I${name}")

    superInterface.foreach { iface =>
      out.print(s" extends I${iface.name}")
    }

    out.println(" {")

    // Abstract fields - common to all the subtypes
    list(fields).foreach { member =>
      out.println(s"${indent}${member.name}: ${getTypeRefString(member.typeRef)};")
    }

    out.println("}")
  }

  private def emitSingletonDeclaration(
    name: String,
    members: ListSet[Member],
    superInterface: Option[InterfaceDeclaration],
    out: PrintStream): Unit = {

    if (members.nonEmpty) {
      def mkString = members.map {
        case Member(nme, tpe, _) => s"$nme ($tpe)"
      }.mkString(", ")

      throw new IllegalStateException(
        s"Cannot emit static members for singleton values: ${mkString}")
    }

    // Class definition
    out.print(s"export class $name")

    superInterface.filter(_ => members.isEmpty).foreach { i =>
      out.print(s" implements ${i.name}")
    }

    out.println(" {")

    out.println(s"${indent}private static instance: $name;\n")

    out.println(s"${indent}private constructor() {}\n")
    out.println(s"${indent}public static getInstance() {")
    out.println(s"${indent(2)}if (!${name}.instance) {")
    out.println(s"${indent(3)}${name}.instance = new ${name}();")
    out.println(s"${indent(2)}}\n")
    out.println(s"${indent(2)}return ${name}.instance;")
    out.println(s"${indent}}")

    if (config.emitCodecs) {
      // Decoder factory: MyClass.fromData({..})
      out.println(s"\n${indent}public static fromData(data: any): ${name} {")
      out.println(s"${indent(2)}return ${name}.instance;")
      out.println(s"${indent}}")

      // Encoder
      out.println(s"\n${indent}public static toData(instance: ${name}): any {")
      out.println(s"${indent(2)}return instance;")
      out.println(s"${indent}}")
    }

    out.println("}")
  }

  private def emitInterfaceDeclaration(
    decl: InterfaceDeclaration,
    out: PrintStream): Unit = {

    val InterfaceDeclaration(name, fields, typeParams, superInterface) = decl

    out.print(s"export interface $name${typeParameters(typeParams)}")

    superInterface.foreach { iface =>
      out.print(s" extends ${iface.name}")
    }

    out.println(" {")
    
    list(fields).foreach { member =>
      out.println(s"${indent}${member.name}: ${getTypeRefString(member.typeRef)};")
    }
    out.println("}")
  }

  private def emitClassDeclaration(
    decl: ClassDeclaration,
    out: PrintStream): Unit = {

    val ClassDeclaration(name, ClassConstructor(parameters),
      values, typeParams, _) = decl

    val tparams = typeParameters(typeParams)

    if (values.nonEmpty) {
      def mkString = values.map {
        case Member(nme, tpe, _) => s"$nme ($tpe)"
      }.mkString(", ")

      throw new IllegalStateException(
        s"Cannot emit static members for class values: ${mkString}")
    }

    // Class definition
    out.print(s"export class ${name}${tparams}")

    if (config.emitInterfaces) {
      out.print(s" implements I${name}${tparams}")
    }

    out.println(" {")

    list(values).foreach { v =>
      out.print(indent)

      if (config.emitInterfaces) {
        out.print("public ")
      }

      out.println(s"${v.name}: ${getTypeRefString(v.typeRef)};")
    }

    val params = list(parameters)

    if (!config.emitInterfaces) {
      // Class fields
      params.foreach { parameter =>
        out.print(s"${indent}public ${parameter.name}: ${getTypeRefString(parameter.typeRef)};")
      }
    }

    // Class constructor
    out.print(s"${indent}constructor(")

    params.zipWithIndex.foreach {
      case (parameter, index) =>
        if (index > 0) {
          out.println(",")
        } else {
          out.println("")
        }

        out.print(s"${indent(2)}")

        if (config.emitInterfaces) {
          out.print("public ")
        }

        out.print(s"${parameter.name}: ${getTypeRefString(parameter.typeRef)}")
    }

    out.println(s"\n${indent}) {")

    params.foreach { parameter =>
      out.println(s"${indent(2)}this.${parameter.name} = ${parameter.name};")
    }
    out.println(s"${indent}}")

    // Codecs functions
    if (config.emitCodecs) {
      emitClassCodecs(decl, out)
    }

    out.println("}")
  }

  private def emitClassCodecs(
    decl: ClassDeclaration,
    out: PrintStream): Unit = {
    import decl.{ constructor, name, typeParams }, constructor.parameters
    val tparams = typeParameters(typeParams)

    if (config.fieldNaming == FieldNaming.Identity) {
      // optimized identity

      // Decoder factory: MyClass.fromData({..})
      out.println(s"\n${indent}public static fromData${tparams}(data: any): ${name}${tparams} {")
      out.println(s"${indent(2)}return <${name}${tparams}>(data);")
      out.println(s"${indent}}")

      // Encoder
      out.println(s"\n${indent}public static toData${tparams}(instance: ${name}${tparams}): any {")
      out.println(s"${indent(2)}return instance;")
      out.println(s"${indent}}")
    } else {
      // Decoder factory: MyClass.fromData({..})
      out.println(s"\n${indent}public static fromData${tparams}(data: any): ${name}${tparams} {")
      out.print(s"${indent(2)}return new ${name}${tparams}(")

      val params = list(parameters).zipWithIndex

      params.foreach {
        case (parameter, index) =>
          val encoded = config.fieldNaming(parameter.name)

          if (index > 0) out.print(", ")

          out.print(s"data.${encoded}")
      }

      out.println(s");")
      out.println(s"${indent}}")

      // Encoder
      out.println(s"\n${indent}public static toData${tparams}(instance: ${name}${tparams}): any {")
      out.println(s"${indent(2)}return {")

      params.foreach {
        case (parameter, index) =>
          val encoded = config.fieldNaming(parameter.name)

          if (index > 0) out.print(",\n")

          out.print(s"${indent(3)}${encoded}: instance.${parameter.name}")
      }

      out.println(s"\n${indent(2)}};")
      out.println(s"${indent}}")
    }
  }
}
