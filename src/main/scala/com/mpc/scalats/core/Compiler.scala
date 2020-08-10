package com.mpc.scalats.core

import scala.collection.immutable.ListSet

import com.mpc.scalats.configuration.Config

import com.mpc.scalats.core.TypeScriptModel.{
  ClassConstructor,
  ClassConstructorParameter,
  CustomTypeRef,
  Declaration,
  InterfaceDeclaration,
  Member,
  NullRef,
  UndefinedRef,
  UnionDeclaration,
  SimpleTypeRef,
  SingletonDeclaration
}

/**
 * Created by Milosz on 09.06.2016.
 */
object Compiler {
  // TODO: Refactor as class with config parameter in ctor

  @inline def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef]
  )(implicit config: Config): ListSet[Declaration] =
    compile(scalaTypes, Option.empty[InterfaceDeclaration])

  def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef],
    superInterface: Option[InterfaceDeclaration]
  )(implicit config: Config): ListSet[Declaration] =
    scalaTypes.flatMap { typeDef =>
      typeDef match {
        case scalaClass: ScalaModel.CaseClass => {
          val clazz = {
            if (config.emitClasses) {
              ListSet[Declaration](compileClass(scalaClass, superInterface))
            } else ListSet.empty[Declaration]
          }

          if (!config.emitInterfaces) clazz
          else ListSet[Declaration](
            compileInterface(scalaClass, superInterface)) ++ clazz
        }

        case ScalaModel.CaseObject(name, members) => {
          val values = members.map { scalaMember =>
            Member(scalaMember.name,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          ListSet[Declaration](
            SingletonDeclaration(name, values, superInterface))
        }

        case ScalaModel.SealedUnion(name, fields, possibilities) => {
          val ifaceFields = fields.map { scalaMember =>
            Member(scalaMember.name,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          val unionRef = InterfaceDeclaration(
            buildInterfaceName(name), ifaceFields, ListSet.empty[String], superInterface)

          compile(possibilities, Some(unionRef)) + UnionDeclaration(
            name,
            ifaceFields,
            possibilities.map {
              case ScalaModel.CaseObject(nme, _) =>
                CustomTypeRef(nme, ListSet.empty)

              case ScalaModel.CaseClass(n, _, _, tpeArgs) =>
                CustomTypeRef(buildInterfaceName(n), tpeArgs.map { SimpleTypeRef(_) })

              case m =>
                CustomTypeRef(buildInterfaceName(m.name), ListSet.empty)
            },
            superInterface)
        }
      }
    }

  private def compileInterface(
    scalaClass: ScalaModel.CaseClass,
    superInterface: Option[InterfaceDeclaration]
  )(implicit config: Config) = InterfaceDeclaration(
    buildInterfaceName(scalaClass.name),
    scalaClass.fields.map { scalaMember =>
      TypeScriptModel.Member(
        scalaMember.name,
        compileTypeRef(scalaMember.typeRef, inInterfaceContext = true),
        scalaMember.value
      )
    },
    typeParams = scalaClass.typeArgs,
    superInterface = superInterface
  )

  private def buildInterfaceName(name: String)(implicit config: Config) = {
    if (config.prependIPrefix) s"I${name}" else name
  }

  private def compileClass(
    scalaClass: ScalaModel.CaseClass,
    superInterface: Option[InterfaceDeclaration])(implicit config: Config) = {
    TypeScriptModel.ClassDeclaration(
      scalaClass.name,
      ClassConstructor(
        scalaClass.fields map { scalaMember =>
          ClassConstructorParameter(
            scalaMember.name,
            compileTypeRef(scalaMember.typeRef, inInterfaceContext = false))
        }
      ),
      values = scalaClass.values.map { v =>
        TypeScriptModel.Member(v.name, compileTypeRef(v.typeRef, false), v.value)
      },
      typeParams = scalaClass.typeArgs,
      superInterface
    )
  }

  private def compileTypeRef(
      scalaTypeRef: ScalaModel.TypeRef,
      inInterfaceContext: Boolean
  )(implicit config: Config): TypeScriptModel.TypeRef = scalaTypeRef match {
    case ScalaModel.IntRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.LongRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.DoubleRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.BooleanRef =>
      TypeScriptModel.BooleanRef
    case ScalaModel.StringRef =>
      TypeScriptModel.StringRef
    case ScalaModel.SeqRef(innerType) =>
      TypeScriptModel.ArrayRef(compileTypeRef(innerType, inInterfaceContext))
    case ScalaModel.NonEmptySeqRef(innerType) =>
      TypeScriptModel.NonEmptyArrayRef(compileTypeRef(innerType, inInterfaceContext))

    case ScalaModel.CaseClassRef(name, typeArgs) => {
      val actualName = if (inInterfaceContext) buildInterfaceName(name) else name
      TypeScriptModel.CustomTypeRef(
        actualName, typeArgs.map(compileTypeRef(_, inInterfaceContext)))
    }

    case ScalaModel.DateRef =>
      TypeScriptModel.DateRef
    case ScalaModel.DateTimeRef =>
      TypeScriptModel.DateTimeRef

    case ScalaModel.TypeParamRef(name) =>
      TypeScriptModel.SimpleTypeRef(name)

    case ScalaModel.OptionRef(innerType) =>
      if (config.optionToNullable && config.optionToUndefined)
        TypeScriptModel.UnionType(ListSet(
          TypeScriptModel.UnionType(ListSet(
            compileTypeRef(innerType, inInterfaceContext), NullRef)),
            UndefinedRef))

      else if (config.emitIoTs)
        TypeScriptModel.CustomTypeRef("optionFromNullable", ListSet(
          compileTypeRef(innerType, inInterfaceContext)))

      else if (config.optionToNullable)
        TypeScriptModel.UnionType(ListSet(
          compileTypeRef(innerType, inInterfaceContext),
          NullRef))

      else if (config.optionToUndefined)
        TypeScriptModel.UnionType(ListSet(
          compileTypeRef(innerType, inInterfaceContext),
          UndefinedRef))

      else
        TypeScriptModel.UnionType(ListSet(
          compileTypeRef(innerType, inInterfaceContext),
          NullRef))

    case ScalaModel.MapRef(kT, vT) => TypeScriptModel.MapType(
      compileTypeRef(kT, inInterfaceContext),
      compileTypeRef(vT, inInterfaceContext))

    case ScalaModel.UnionRef(possibilities) =>
      TypeScriptModel.UnionType(possibilities.map { i =>
        compileTypeRef(i, inInterfaceContext)
      })

    case ScalaModel.TupleRef(tpes) =>
      TypeScriptModel.TupleType(tpes.map(compileTypeRef(_, inInterfaceContext)))

    case ScalaModel.TheseRef(lT, rT) =>
      TypeScriptModel.TheseType(
        compileTypeRef(lT, inInterfaceContext),
        compileTypeRef(rT, inInterfaceContext))

    case ScalaModel.UnknownTypeRef(u) =>
      TypeScriptModel.UnknownTypeRef(u)
  }
}
