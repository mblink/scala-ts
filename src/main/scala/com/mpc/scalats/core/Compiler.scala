package com.mpc.scalats.core

import cats.{Apply, Eval, Functor}
import cats.syntax.foldable._
import cats.syntax.traverseFilter._
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
    case ScalaModel.BigDecimalRef =>
      TypeScriptModel.BigNumberRef
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

  private implicit class BooleanOps(b: Boolean) {
    def andM[M[_]](mb: M[Boolean])(implicit M: Functor[M]): M[Boolean] = M.map(mb)(b && _)
    def orM[M[_]](mb: M[Boolean])(implicit M: Functor[M]): M[Boolean] = M.map(mb)(b || _)
  }

  private implicit class MBooleanOps[M[_]](b: M[Boolean]) {
    def orM(mb: M[Boolean])(implicit M: Apply[M]): M[Boolean] = M.map2(b, mb)(_ || _)
  }

  private def sortTypes(decls: ListSet[TypeScriptModel.Declaration]): ListSet[TypeScriptModel.Declaration] = {
    def refersToRef(r: TypeScriptModel.TypeRef, d: TypeScriptModel.Declaration, filterUnionName: String => Option[String]): Eval[Boolean] =
      r match {
        case TypeScriptModel.CustomTypeRef(n, rs, _) => (n == d.name).orM(rs.toList.existsM(refersToRef(_, d, filterUnionName)))
        case TypeScriptModel.ArrayRef(r) => refersToRef(r, d, filterUnionName)
        case TypeScriptModel.SetRef(r) => refersToRef(r, d, filterUnionName)
        case TypeScriptModel.NonEmptyArrayRef(r) => refersToRef(r, d, filterUnionName)
        case TypeScriptModel.UnknownTypeRef(n) => Eval.now(n == d.name)
        case TypeScriptModel.SimpleTypeRef(n) => Eval.now(n == d.name)
        case TypeScriptModel.UnionType(n, typeParams, _, _) =>
          (filterUnionName(n) == Some(d.name)).orM(typeParams.toList.existsM(refersToRef(_, d, filterUnionName)))
        case TypeScriptModel.OptionType(i) => refersToRef(i, d, filterUnionName)
        case TypeScriptModel.EitherType(l, r) => refersToRef(l, d, filterUnionName).orM(refersToRef(r, d, filterUnionName))
        case TypeScriptModel.TheseType(l, r) => refersToRef(l, d, filterUnionName).orM(refersToRef(r, d, filterUnionName))
        case TypeScriptModel.MapType(k, v) => refersToRef(k, d, filterUnionName).orM(refersToRef(v, d, filterUnionName))
        case TypeScriptModel.TupleType(rs) => rs.toList.existsM(refersToRef(_, d, filterUnionName))
        case TypeScriptModel.NumberRef |
             TypeScriptModel.BigNumberRef |
             TypeScriptModel.StringRef |
             TypeScriptModel.BooleanRef |
             TypeScriptModel.DateRef |
             TypeScriptModel.DateTimeRef |
             TypeScriptModel.NullRef |
             TypeScriptModel.UndefinedRef => Eval.now(false)
      }

    def refersTo(d1: TypeScriptModel.Declaration, d2: TypeScriptModel.Declaration): Eval[Boolean] =
      d1 match {
        case TypeScriptModel.InterfaceDeclaration(_, fs, _, si) =>
          fs.toList.existsM(f => refersToRef(f.typeRef, d2, Some(_))).flatMap { fsRef =>
            if (si == d2.superInterface && !fsRef) Eval.now(false)
            else fsRef.orM(si.fold(Eval.now(false))(refersTo(_, d2)))
          }

        case TypeScriptModel.SingletonDeclaration(_, vs, si) =>
          vs.toList.existsM(v => refersToRef(v.typeRef, d2, Some(_))).flatMap { vsRef =>
            if (si == d2.superInterface && !vsRef) Eval.now(false)
            else vsRef.orM(si.fold(Eval.now(false))(refersTo(_, d2)))
          }

        case TypeScriptModel.UnionDeclaration(n, fs, _, ps, si, _) =>
          fs.toList.existsM(f => refersToRef(f.typeRef, d2, Some(_).filterNot(_ == n)))
            .orM(ps.toList.existsM(refersToRef(_, d2, Some(_).filterNot(_ == n))))
            .orM(si.fold(Eval.now(false))(refersTo(_, d2)))

        case TypeScriptModel.TypeDeclaration(_, r, _) =>
          refersToRef(r, d2, Some(_))

        case TypeScriptModel.TaggedTypeDeclaration(_, r, _) =>
          refersToRef(r, d2, Some(_))
      }

    val distinctDecls = decls.groupBy(_.name).map(_._2.head).foldLeft(ListSet[TypeScriptModel.Declaration]())(_ + _)

    def addDecl(acc: Eval[ListSet[TypeScriptModel.Declaration]], decl: TypeScriptModel.Declaration): Eval[ListSet[TypeScriptModel.Declaration]] =
      acc.flatMap { l =>
        // If this decl is already in the result set, we don't need to do anything
        if (l.exists(_.name == decl.name))
          Eval.now(l)
        else
          // Otherwise, get the list of decls that this decl refers to, add each of them first, then add this decl
          for {
            referredToDecls <- distinctDecls.toList.traverseFilter(d =>
              (d.name != decl.name).andM(refersTo(decl, d)).map(b => if (b) Some(d) else None))
            res <- referredToDecls.foldLeft(Eval.now(l))(addDecl).map(_ + decl)
          } yield res
      }

    distinctDecls.foldLeft(Eval.now(ListSet[TypeScriptModel.Declaration]()))(addDecl).value
  }
}
