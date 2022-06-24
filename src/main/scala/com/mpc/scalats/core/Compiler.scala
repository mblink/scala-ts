package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel.{
  CustomTypeRef,
  Declaration,
  InterfaceDeclaration,
  Member,
  UnionDeclaration,
  SimpleTypeRef,
  SingletonDeclaration
}
import scala.collection.immutable.ListSet

case class Compiler(config: Config) {
  @inline def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef]
  ): ListSet[Declaration] =
    sortTypes(compile(scalaTypes, None))

  def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef],
    superInterface: Option[UnionDeclaration]
  ): ListSet[Declaration] =
    scalaTypes.flatMap { typeDef =>
      typeDef match {
        case scalaClass: ScalaModel.CaseClass =>
          ListSet[Declaration](compileInterface(scalaClass, superInterface))

        case ScalaModel.CaseObject(name, _, members, _) =>
          val values = members.map { scalaMember =>
            Member(scalaMember.name.trim,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          ListSet[Declaration](
            SingletonDeclaration(name, values, superInterface))

        case ScalaModel.SealedUnion(name, _, fields, typeArgs, possibilities, _) =>
          val ifaceFields = fields.map { scalaMember =>
            Member(scalaMember.name.trim,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          val unionDecl = UnionDeclaration(
            name,
            ifaceFields,
            typeArgs,
            possibilities.map {
              case ScalaModel.CaseObject(nme, _, _, scalaType) =>
                CustomTypeRef(nme, ListSet.empty, scalaType)

              case ScalaModel.CaseClass(n, fn, _, _, tpeArgs, scalaType) =>
                CustomTypeRef(buildTypeName(n, fn), tpeArgs.map { SimpleTypeRef(_) }, scalaType)

              case m =>
                CustomTypeRef(buildTypeName(m.name, m.fullTypeName), ListSet.empty, m.scalaType)
            },
            superInterface,
            possibilities.forall(_ match {
              case _: ScalaModel.CaseObject => true
              case _ => false
            }))

          compile(possibilities, Some(unionDecl)) + unionDecl

        case typeAlias: ScalaModel.TypeAlias =>
          ListSet[Declaration](compileTypeAlias(typeAlias))

        case taggedType: ScalaModel.TaggedType =>
          ListSet[Declaration](compileTaggedType(taggedType))
      }
    }

  private def compileInterface(
    scalaClass: ScalaModel.CaseClass,
    superInterface: Option[UnionDeclaration]
  ) = InterfaceDeclaration(
    buildTypeName(scalaClass.name, scalaClass.fullTypeName),
    scalaClass.fields.map { scalaMember =>
      TypeScriptModel.Member(
        scalaMember.name.trim,
        compileTypeRef(scalaMember.typeRef, inInterfaceContext = true),
        scalaMember.value
      )
    },
    typeParams = scalaClass.typeArgs,
    superInterface = superInterface
  )

  private def buildTypeName(name: String, fullTypeName: String) = {
    val base = if (config.prependIPrefix) s"I${name}" else name
    if (fullTypeName.endsWith("[Option]")) s"${base}Opt" else base
  }

  private def compileTypeAlias(
    scalaTypeAlias: ScalaModel.TypeAlias
  ): TypeScriptModel.TypeDeclaration =
    TypeScriptModel.TypeDeclaration(
      buildTypeName(scalaTypeAlias.name, scalaTypeAlias.fullTypeName),
      compileTypeRef(scalaTypeAlias.typeRef, false),
      scalaTypeAlias.typeArgs)

  private def compileTaggedType(
    scalaTaggedType: ScalaModel.TaggedType
  ): TypeScriptModel.TaggedTypeDeclaration =
    TypeScriptModel.TaggedTypeDeclaration(
      buildTypeName(scalaTaggedType.name, scalaTaggedType.fullTypeName),
      compileTypeRef(scalaTaggedType.baseTypeRef, false),
      scalaTaggedType.tagTypeName)

  private def compileTypeRef(
    scalaTypeRef: ScalaModel.TypeRef,
    inInterfaceContext: Boolean
  ): TypeScriptModel.TypeRef = scalaTypeRef match {
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
    case ScalaModel.SetRef(innerType) =>
      TypeScriptModel.SetRef(compileTypeRef(innerType, inInterfaceContext))
    case ScalaModel.NonEmptySeqRef(innerType) =>
      TypeScriptModel.NonEmptyArrayRef(compileTypeRef(innerType, inInterfaceContext))

    case ScalaModel.CaseClassRef(name, fullTypeName, typeArgs, scalaType) => {
      val actualName = if (inInterfaceContext) buildTypeName(name, fullTypeName) else name
      TypeScriptModel.CustomTypeRef(actualName, typeArgs.map(compileTypeRef(_, inInterfaceContext)), scalaType)
    }

    case ScalaModel.DateRef =>
      TypeScriptModel.DateRef
    case ScalaModel.DateTimeRef =>
      TypeScriptModel.DateTimeRef

    case ScalaModel.TypeParamRef(name, _) =>
      TypeScriptModel.SimpleTypeRef(name)

    case ScalaModel.TaggedTypeRef(name, scalaType) =>
      TypeScriptModel.CustomTypeRef(name, ListSet(), scalaType)

    case ScalaModel.OptionRef(innerType) =>
      TypeScriptModel.OptionType(compileTypeRef(innerType, inInterfaceContext))

    case ScalaModel.MapRef(kT, vT) => TypeScriptModel.MapType(
      compileTypeRef(kT, inInterfaceContext),
      compileTypeRef(vT, inInterfaceContext))

    case ScalaModel.UnionRef(name, typeArgs, possibilities, scalaType) =>
      TypeScriptModel.UnionType(
        name,
        typeArgs.map(compileTypeRef(_, inInterfaceContext)),
        possibilities.map(compileTypeRef(_, inInterfaceContext)),
        scalaType)

    case ScalaModel.TupleRef(tpes) =>
      TypeScriptModel.TupleType(tpes.map(compileTypeRef(_, inInterfaceContext)))

    case ScalaModel.EitherRef(lT, rT) =>
      TypeScriptModel.EitherType(compileTypeRef(lT, inInterfaceContext), compileTypeRef(rT, inInterfaceContext))

    case ScalaModel.TheseRef(lT, rT) =>
      TypeScriptModel.TheseType(
        compileTypeRef(lT, inInterfaceContext),
        compileTypeRef(rT, inInterfaceContext))

    case ScalaModel.UnknownTypeRef(u, _) =>
      TypeScriptModel.UnknownTypeRef(u)
  }

  private def sortTypes(decls: ListSet[TypeScriptModel.Declaration]): ListSet[TypeScriptModel.Declaration] = {
    def refersToRef(r: TypeScriptModel.TypeRef, d: TypeScriptModel.Declaration): Boolean =
      r match {
        case TypeScriptModel.CustomTypeRef(n, rs, _) => n == d.name || rs.exists(refersToRef(_, d))
        case TypeScriptModel.ArrayRef(r) => refersToRef(r, d)
        case TypeScriptModel.SetRef(r) => refersToRef(r, d)
        case TypeScriptModel.NonEmptyArrayRef(r) => refersToRef(r, d)
        case TypeScriptModel.UnknownTypeRef(n) => n == d.name
        case TypeScriptModel.SimpleTypeRef(n) => n == d.name
        case TypeScriptModel.UnionType(_, ps, rs, _) => ps.exists(refersToRef(_, d)) || rs.exists(refersToRef(_, d))
        case TypeScriptModel.OptionType(i) => refersToRef(i, d)
        case TypeScriptModel.EitherType(l, r) => refersToRef(l, d) || refersToRef(r, d)
        case TypeScriptModel.TheseType(l, r) => refersToRef(l, d) || refersToRef(r, d)
        case TypeScriptModel.MapType(k, v) => refersToRef(k, d) || refersToRef(v, d)
        case TypeScriptModel.TupleType(rs) => rs.exists(refersToRef(_, d))
        case TypeScriptModel.NumberRef | TypeScriptModel.StringRef | TypeScriptModel.BooleanRef |
             TypeScriptModel.DateRef | TypeScriptModel.DateTimeRef | TypeScriptModel.NullRef |
             TypeScriptModel.UndefinedRef => false
      }

    def refersTo(d1: TypeScriptModel.Declaration, d2: TypeScriptModel.Declaration): Boolean =
      d1 match {
        case TypeScriptModel.InterfaceDeclaration(_, fs, _, si) =>
          fs.exists(f => refersToRef(f.typeRef, d2)) || si.fold(false)(refersTo(_, d2))

        case TypeScriptModel.SingletonDeclaration(_, vs, si) =>
          vs.exists(v => refersToRef(v.typeRef, d2)) || si.fold(false)(refersTo(_, d2))

        case TypeScriptModel.UnionDeclaration(_, fs, _, ps, si, _) =>
          fs.exists(f => refersToRef(f.typeRef, d2)) || ps.exists(refersToRef(_, d2)) || si.fold(false)(refersTo(_, d2))

        case TypeScriptModel.TypeDeclaration(_, r, _) =>
          refersToRef(r, d2)

        case TypeScriptModel.TaggedTypeDeclaration(_, r, _) =>
          refersToRef(r, d2)
      }

    decls.foldLeft((ListSet[TypeScriptModel.Declaration](), Set[String]())) { case ((acc, seen), decl) =>
      if (seen.contains(decl.name)) {
        (acc, seen)
      } else {
        val (refersToDecl, doesntReferToDecl) = acc.span(refersTo(_, decl))
        (doesntReferToDecl ++ ListSet(decl) ++ refersToDecl, seen + decl.name)
      }
    }._1
  }
}
