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
import scala.reflect.runtime.universe.typeOf

case class Compiler(config: Config) {
  @inline def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef]
  ): ListSet[Declaration] =
    sortTypes(compile(scalaTypes, None))

  def compile(
    scalaTypes: ListSet[ScalaModel.TypeDef],
    superInterface: Option[InterfaceDeclaration]
  ): ListSet[Declaration] =
    scalaTypes.flatMap { typeDef =>
      typeDef match {
        case scalaClass: ScalaModel.CaseClass =>
          ListSet[Declaration](compileInterface(scalaClass, superInterface))

        case ScalaModel.CaseObject(name, _, members, _) =>
          val values = members.map { scalaMember =>
            Member(scalaMember.name,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          ListSet[Declaration](
            SingletonDeclaration(name, values, superInterface))

        case ScalaModel.SealedUnion(name, fullTypeName, fields, possibilities, _) =>
          val ifaceFields = fields.map { scalaMember =>
            Member(scalaMember.name,
              compileTypeRef(scalaMember.typeRef, false),
              scalaMember.value)
          }

          val unionRef = InterfaceDeclaration(
            buildTypeName(name, fullTypeName), ifaceFields, ListSet.empty[String], superInterface)

          compile(possibilities, Some(unionRef)) + UnionDeclaration(
            name,
            ifaceFields,
            possibilities.map {
              case ScalaModel.CaseObject(nme, _, _, scalaType) =>
                CustomTypeRef(nme, ListSet.empty, scalaType)

              case ScalaModel.CaseClass(n, fn, _, _, tpeArgs, scalaType) =>
                CustomTypeRef(buildTypeName(n, fn), tpeArgs.map { SimpleTypeRef(_) }, scalaType)

              case m =>
                CustomTypeRef(buildTypeName(m.name, m.fullTypeName), ListSet.empty, m.scalaType)
            },
            superInterface,
            possibilities.foldLeft(true)((b, p) =>
              if(b) {
                p match {
                  case ScalaModel.CaseObject(_, _, _, _) => true
                  case _ => false
                }
              } else {
                false
              }))

        case typeAlias: ScalaModel.TypeAlias =>
          ListSet[Declaration](compileTypeAlias(typeAlias))
      }
    }

  private def compileInterface(
    scalaClass: ScalaModel.CaseClass,
    superInterface: Option[InterfaceDeclaration]
  ) = InterfaceDeclaration(
    buildTypeName(scalaClass.name, scalaClass.fullTypeName),
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

    case ScalaModel.OptionRef(innerType) =>
      TypeScriptModel.CustomTypeRef("optionFromNullable", ListSet(
        compileTypeRef(innerType, inInterfaceContext)), typeOf[Option[_]].typeConstructor)

    case ScalaModel.MapRef(kT, vT) => TypeScriptModel.MapType(
      compileTypeRef(kT, inInterfaceContext),
      compileTypeRef(vT, inInterfaceContext))

    case ScalaModel.UnionRef(possibilities) =>
      TypeScriptModel.UnionType(possibilities.map { i =>
        compileTypeRef(i, inInterfaceContext)
      })

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
        case TypeScriptModel.NonEmptyArrayRef(r) => refersToRef(r, d)
        case TypeScriptModel.UnknownTypeRef(n) => n == d.name
        case TypeScriptModel.SimpleTypeRef(n) => n == d.name
        case TypeScriptModel.UnionType(rs) => rs.exists(refersToRef(_, d))
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

        case TypeScriptModel.UnionDeclaration(_, fs, ps, si, _) =>
          fs.exists(f => refersToRef(f.typeRef, d2)) || ps.exists(refersToRef(_, d2)) || si.fold(false)(refersTo(_, d2))

        case TypeScriptModel.TypeDeclaration(_, r, _) =>
          refersToRef(r, d2)
      }

    var seen = Set[String]()
    var res = ListSet[TypeScriptModel.Declaration]()
    var curr = 0

    def addDecl(decl: TypeScriptModel.Declaration): Unit =
      if (seen.contains(decl.name)) ()
      else {
        seen = seen + decl.name
        // Any declarations that this declaration refers to go first
        decls.filter(refersTo(decl, _)).foreach(addDecl)
        // then this declaration
        res += decl
      }

    while (math.max(res.size, curr) < decls.size) {
      addDecl(decls.toList(curr))
      curr += 1
    }

    res
  }
}
