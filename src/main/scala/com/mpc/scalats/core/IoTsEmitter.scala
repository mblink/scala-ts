package com.mpc.scalats
package core

import cats.data.{NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.syntax.foldable._
import com.mpc.scalats.configuration.Config
import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe.Type
import scala.util.Try

final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._

  private def typeAsValArg(s: String): String = s"_${s}val"

  def emit(declarations: ListSet[Declaration])(implicit ctx: TsImports.Ctx): Lines =
    declarations.toList.foldMap {
      case decl: InterfaceDeclaration =>
        emitIoTsInterfaceDeclaration(decl)
      case decl: UnionDeclaration =>
        emitUnionDeclaration(decl)
      case decl: SingletonDeclaration =>
        emitSingletonDeclaration(decl)
      case decl: TypeDeclaration =>
        emitTypeDeclaration(decl)
      case decl: TaggedTypeDeclaration =>
        emitTaggedTypeDeclaration(decl)
    }

  private def iotsTypeParams(typeParams: ListSet[String]): TsImports.With[String] =
    typeParams.joinTypeParams(p => s"$p extends " |+| imports.iotsMixed)

  private def iotsTypeParamVals(typeParams: ListSet[String]): TsImports.With[String] =
    typeParams.joinParens(", ")(p => s"${typeAsValArg(p)}: $p")

  private def iotsTypeParamArgs(typeParams: ListSet[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.emptyStr)
    else iotsTypeParams(typeParams) |+| iotsTypeParamVals(typeParams) |+| " => "

  private case class CodecCtx(
    codecName: String,
    tpeName: String,
    typeParams: ListSet[String],
    modType: String => String
  ) {
    def cType: String = modType(codecType(tpeName))
    def className = s"${codecName}C"
    def tps: ListSet[String] = typeParams
    def hasTps: Boolean = typeParams.nonEmpty
    def tpe: TsImports.With[String] = modType(tpeName) |+| typeParams.joinTypeParams(identity)
    def classTpe: TsImports.With[String] = className |+| typeParams.joinTypeParams(identity)

    def codecValDef(body: CodecCtx => Lines): Lines =
      "export " +: (if (hasTps) (s"class $className" |+| iotsTypeParams(tps) |+|
        " { codec = " |+| iotsTypeParamVals(typeParams) |+| " => ") +: body(this) :+ "};"
      else s"const $codecName = " +: body(this) :+ ";") |+|
      (if (hasTps) line(s"export const $codecName = " |+| iotsTypeParamArgs(tps) |+|
        "new " |+| classTpe |+| "().codec" |+| tps.joinParens(", ")(typeAsValArg) |+| ";") else Nil)

    def codecTypeDef: Lines =
      line(s"export type $cType" |+| iotsTypeParams(tps) |+| " = " |+| (
        if (hasTps) "ReturnType<" |+| classTpe |+| """["codec"]>;"""
        else s"typeof $codecName;"
      ))

    def typeDefPrefix: TsImports.With[String] =
      "export type " |+| tpe |+| " = "

    def typeDef: Lines =
      line(typeDefPrefix |+| imports.iotsTypeOf(cType |+| tps.joinTypeParams(imports.iotsTypeType |+| "<" |+| _ |+| ">")) |+| ";")

    def emit(body: CodecCtx => Lines): Lines =
      codecValDef(body) |+|
      codecTypeDef |+|
      typeDef

    def withNames(cName: String, tName: String): CodecCtx =
      copy(codecName = cName, tpeName = tName)
  }

  private def membersCodec(members: ListSet[Member])(
    onNoVal: TypeRef => TsImports.With[String],
    modIndent: String => String = identity,
    wrapDef: Lines => Lines = identity,
  )(implicit ctx: TsImports.Ctx): CodecCtx => Lines =
    _ => imports.iotsTypeFunction.lines(
      _ ++ "{",
      wrapDef(emitMembers(members, getIoTsTypeWrappedVal, onNoVal, modIndent = modIndent)),
      "}" ++ _)

  private def unionCodec(possibilities: ListSet[CustomTypeRef])(implicit ctx: TsImports.Ctx): CodecCtx => Lines = c => {
    val memberCodec = (t: CustomTypeRef) => codecName(t.name) |+|
      (if (t.typeArgs.isEmpty) "" else t.typeArgs.joinParens(", ")(getIoTsTypeString))

    if (possibilities.size == 1) line(memberCodec(possibilities.head))
    else if (possibilities.size == 0) sys.error(s"Can't create ${c.codecName} union composed of 0 members")
    else line(imports.iotsUnion(possibilities.joinArray(memberCodec)))
  }

  private def emitCodec0(
    name: String,
    tpeName: String,
    typeParams: ListSet[String],
    mkCodecName: String => String = codecName,
    modType: String => String = identity
  )(emit: CodecCtx => Lines): Lines =
    emit(CodecCtx(mkCodecName(name), tpeName, typeParams, modType))

  private def emitCodec(
    name: String,
    tpeName: String,
    typeParams: ListSet[String],
    mkCodecName: String => String = codecName,
    modType: String => String = identity
  )(body: CodecCtx => Lines): Lines =
    emitCodec0(name, tpeName, typeParams, mkCodecName, modType)(_.emit(body))

  private def _tagMember(name: String): Member = Member("_tag", StringRef, Some(name))

  private def emitImappedCodec(
    origName: String,
    origTpeName: String,
    newName: String,
    newTpeName: String,
    typeParams: ListSet[String],
    superInterface: Option[UnionDeclaration]
  )(newToOld: String => TsImports.With[String], oldToNew: String => TsImports.With[String]): Lines = {
    val (origCName, newCName) = (codecName(origName), codecName(newName))
    val tpeOfTps = typeParams.joinTypeParams(imports.iotsTypeOf(_))
    val (origTpeOfTpe, newTpeOfTpe) = (origTpeName |+| tpeOfTps, newTpeName |+| tpeOfTps)

    imports.fptsPipe.lines(
      s"export const $newCName = " |+| iotsTypeParamArgs(typeParams) |+| _ |+| origCName |+|
        (if (typeParams.nonEmpty) typeParams.joinParens(", ")(typeAsValArg) else "") |+|
        ", c => new " |+| imports.iotsTypeType |+| List(newTpeOfTpe, origTpeOfTpe).joinTypeParams(identity) |+| "(",
      line(s"""$indent"${superInterface.fold("")(_.name ++ " ")}$newName",""") |+|
      line(s"$indent(u: unknown): u is " |+| newTpeOfTpe |+| " => " |+| imports.fptsEither("isRight(c.decode(u))") |+| ",") |+|
      line(s"$indent(u: unknown): " |+|
        imports.fptsEither("Either" |+| List(imports.iotsErrors, newTpeOfTpe).joinTypeParams(identity)) |+| " => " |+|
        imports.fptsPipe("c.decode(u), " |+| imports.fptsEither("map(x => " |+| oldToNew("x") |+| ")")) |+| ",") |+|
      line(s"$indent(x: " |+| newTpeOfTpe |+| "): " |+| origTpeOfTpe |+| " => " |+| newToOld("x")),
      ")" ++ _ ++ ";")
  }

  private def emitIoTsInterfaceDeclaration(decl: InterfaceDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val InterfaceDeclaration(name, fields, typeParams, superInterface) = decl

    emitCodec(name, name, typeParams)(membersCodec(fields ++ superInterface.map(_ => _tagMember(name)))(getIoTsTypeString)) |+|
    line()
  }

  private def tsUnionName(name: String): String = s"${codecType(name)}U"

  private def emitUnionDeclaration(decl: UnionDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val UnionDeclaration(name, _, typeParams, possibilities, _, emitAll) = decl

    def union(nameFn: CustomTypeRef => TsImports.With[String], prefix: String, glue: String, suffix: String) =
      possibilities.join(prefix, glue, suffix)(nameFn)

    val memberCodec = (t: CustomTypeRef) => codecName(t.name) |+|
      (if (t.typeArgs.isEmpty) "" else t.typeArgs.joinParens(", ")(getIoTsTypeString))
    val codecs = union(memberCodec, "[", ", ", "]")
    val (allCName, allNamesConst, nameType, ordInst, tpeMap) =
      (s"all${name}C", s"all${name}Names", s"${name}Name", unionOrdName(name), s"${name}Map")

    line(s"export const $allCName = " |+| iotsTypeParamArgs(typeParams) |+| codecs |+| " as const;") |+|
    line(s"export const $allNamesConst = " |+| possibilities.joinArray(p => s""""${p.name}"""") |+| " as const;") |+|
    line(s"export type $nameType = (typeof $allNamesConst)[number];") |+|
    emitCodec(name, name, typeParams, tsUnionName, _ ++ "U")(unionCodec(possibilities)) |+|
    (if (typeParams.isEmpty) line(s"export const $ordInst: " |+| imports.fptsOrd(s"Ord<${name}U>") |+| " = " |+| iotsTypeParamArgs(typeParams) |+| imports.fptsPipe(
      imports.fptsString("Ord", Some("stringOrd")) |+| ", " |+| imports.fptsOrd("contramap(x => x._tag)")
    )) else Nil) |+|
    (if (emitAll) line(s"export const all$name = " |+| union(p => objectName(p.name), "[", ", ", "]") |+| " as const;") else Nil) |+|
    (if (typeParams.isEmpty) line(s"export type $tpeMap<A> = { [K in $nameType]: A };") else Nil) |+|
    line()
  }

  private def emitSingletonDeclaration(decl: SingletonDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val SingletonDeclaration(name, members, superInterface) = decl
    val (ifaceName, objName, taggedName) = (interfaceName(name), objectName(name), s"${name}Tagged")
    val (taggedIfaceName, tagMmbr) = (interfaceName(taggedName), _tagMember(name))

    line(s"export const $objName = {") |+|
    emitMembers(members ++ superInterface.map(_ => tagMmbr), getTypeWrappedVal(_, _, false)) |+|
    line("} as const;") |+|
    line() |+|
    superInterface.fold(
      emitCodec(name, interfaceName(name), ListSet.empty)(membersCodec(members)(getTypeRefString)))(i =>
      emitCodec0(taggedName, taggedIfaceName, ListSet())(c =>
        c.emit(membersCodec(ListSet(tagMmbr))(getTypeRefString)) |+|
        line(c.withNames(name, ifaceName).typeDefPrefix |+| c.tpe |+| s" & typeof $objName; ")) |+|
      emitImappedCodec(taggedName, taggedIfaceName, name, ifaceName, ListSet(),Some(i))(
        s => s"""({ ...$s, ${tagMmbr.name}: "$name" })""", s => s"({ ...$s, ...$objName })")) |+|
    line()
  }

  private def emitTypeDeclaration(decl: TypeDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val TypeDeclaration(name, typeRef, typeParams) = decl
    val typeVal = codecName(name)

    line(s"export const $typeVal = " |+| iotsTypeParamArgs(typeParams) |+| getIoTsTypeString(typeRef) |+| ";") |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf(s"typeof $typeVal") |+| ";") else Nil ) |+|
    line()
  }

  private def emitTaggedTypeDeclaration(decl: TaggedTypeDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val TaggedTypeDeclaration(name, baseTypeRef, tagTypeName) = decl
    val underlyingCodecName = s"${codecName(name)}Underlying"

    line(s"export interface $tagTypeName { readonly $tagTypeName: unique symbol; }") |+|
    line(s"const $underlyingCodecName = " |+| getIoTsTypeString(baseTypeRef) |+| ";") |+|
    emitCodec(name, name, ListSet())(_ => imports.iotsBrand.lines(
      identity,
      line("  " |+| getIoTsTypeString(baseTypeRef) |+| ",") |+|
      line("  (x): x is " |+| imports.iotsBrandedType(
        imports.iotsTypeOf(s"typeof $underlyingCodecName"), tagTypeName) |+| " => " |+| underlyingCodecName |+| ".is(x),") |+|
      line(s"""  "$tagTypeName""""),
      _ ++ ";"
    ))
  }

  private def emitMembers(
    members: ListSet[Member],
    onVal: (Any, TypeRef) => TsImports.With[String],
    onNoVal: TypeRef => TsImports.With[String],
    modIndent: String => String
  ): Lines =
    members.joinLines(",")(m => s"${modIndent(indent)}${m.name}: " |+|
      m.value.fold(onNoVal(m.typeRef))(onVal(_, m.typeRef)))

  private def emitMembers(
    members: ListSet[Member],
    onVal: (Any, TypeRef) => TsImports.With[String]
  )(implicit ctx: TsImports.Ctx): Lines =
    emitMembers(members, onVal, getTypeRefString, identity)

  private def getIoTsTypeString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumber
    case BooleanRef => imports.iotsBoolean
    case StringRef => imports.iotsString
    case DateRef => imports.iotsLocalDate
    case DateTimeRef => imports.iotsDateTime
    case ArrayRef(innerType) => imports.iotsReadonlyArray(getIoTsTypeString(innerType))
    case SetRef(innerType) =>
      imports.iotsReadonlySetFromArray(getIoTsTypeString(innerType), imports.fptsOrdInstance(innerType))
    case NonEmptyArrayRef(innerType) => imports.iotsReadonlyNonEmptyArray(getIoTsTypeString(innerType))
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => (imports.empty, unknown)
    case SimpleTypeRef(param) => (imports.empty, typeAsValArg(param))
    case UnionType(name, typeParams, possibilities, scalaType) =>
      imports.custom(scalaType, tsUnionName(name))
        .getOrElse(imports.iotsUnion(possibilities.joinArray(getIoTsTypeString))) |+|
        (if (typeParams.isEmpty) imports.emptyStr else typeParams.joinParens(", ")(getIoTsTypeString))
    case OptionType(iT) =>
      imports.iotsOption(getIoTsTypeString(iT))
    case EitherType(lT, rT) =>
      imports.iotsEither(List(lT, rT).join(", ")(getIoTsTypeString))
    case TheseType(lT, rT) =>
      imports.iotsThese(List(lT, rT).join(", ")(getIoTsTypeString))
    case MapType(keyType, valueType) =>
      imports.iotsRecord(getIoTsRecordKeyTypeString(keyType) |+| ", " |+| getIoTsTypeString(valueType))
    case TupleType(types) =>
      imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => imports.iotsNull
    case UndefinedRef => imports.iotsUndefined
  }

  private def getIoTsRecordKeyTypeString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumberFromString
    case _ => getIoTsTypeString(typeRef)
  }

  private def getIoTsTypeWrappedVal(value: Any, typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsLiteral(value.toString)
    case BooleanRef => imports.iotsLiteral(value.toString)
    case StringRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateTimeRef => imports.iotsLiteral(s"new Date(`${value.toString.trim}`)")
    case ArrayRef(_) => imports.iotsLiteral(s"[$value]")
    case SetRef(typeRef) => imports.iotsLiteral(imports.fptsReadonlySet(
      s"fromReadonlyArray(" |+| imports.fptsOrdInstance(typeRef) |+| s")([$value])"
    ))
    case NonEmptyArrayRef(_) => imports.iotsLiteral(imports.iotsReadonlyNonEmptyArray.value |+| s".of([$value])")
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeWrappedVal(value, _)))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => imports.iotsStrict(typeAsValArg(param))
    case UnionType(name, typeParams, possibilities, scalaType) =>
      imports.custom(scalaType, name)
        .getOrElse(imports.iotsStrict(imports.iotsUnion(possibilities.joinArray(getIoTsTypeWrappedVal(value, _))))) |+|
        (if (typeParams.isEmpty) imports.emptyStr else typeParams.joinParens(", ")(getIoTsTypeWrappedVal(value, _)))
    case OptionType(iT) =>
      imports.iotsOption(getIoTsTypeWrappedVal(value, iT))
    case EitherType(lT, rT) =>
      imports.iotsStrict(imports.iotsUnion(
        List(lT, rT).joinArray(getIoTsTypeWrappedVal(value, _))))
    case TheseType(lT, rT) =>
      imports.iotsStrict(imports.iotsUnion(
        List(lT, rT, TupleType(ListSet(lT, rT))).joinArray(getIoTsTypeWrappedVal(value, _))))
    case MapType(keyType, valueType) =>
      imports.iotsStrict(imports.iotsRecord(
        List(keyType, valueType).join(", ")(getIoTsTypeWrappedVal(value, _))))
    case TupleType(types) =>
      imports.iotsStrict(imports.iotsTuple(
        types.joinArray(getIoTsTypeWrappedVal(value, _))))
    case NullRef => imports.iotsLiteral("null")
    case UndefinedRef => imports.iotsLiteral("undefined")
  }

  private def customIoTsTypes(scalaType: Type, name: String, customName: String => String)(implicit ctx: TsImports.Ctx): TsImports.With[String] = name match {
    case "NonEmptyChain" | "NonEmptyList" | "NonEmptyVector" => imports.iotsReadonlyNonEmptyArray.value
    case _ => imports.custom(scalaType, customName(name)).getOrElse(imports.lift(customName(name)))
  }

  @tailrec private def normalizeEval(value: Any): Any = value match {
    case e: cats.Eval[_] => normalizeEval(e.value)
    case v => v
  }

  private def getTypeWrappedVal(value0: Any, typeRef: TypeRef, interfaceContext: Boolean)(implicit ctx: TsImports.Ctx): TsImports.With[String] = {
    val value = normalizeEval(value0)
    typeRef match {
      case NumberRef => value.toString
      case BooleanRef => value.toString
      case StringRef => s"`${value.toString.trim}`"
      case DateRef => s"`${value.toString.trim}`"
      case DateTimeRef => s"`${value.toString.trim}`"
      case ArrayRef(t) => value.asInstanceOf[Iterable[Any]].joinArray(getTypeWrappedVal(_, t, interfaceContext))
      case SetRef(t) => imports.fptsReadonlySet(s"fromReadonlyArray(" |+| imports.fptsOrdInstance(t) |+| s")(" |+|
        value.asInstanceOf[Iterable[Any]].joinArray(getTypeWrappedVal(_, t, interfaceContext)) |+|
      ")")
      case NonEmptyArrayRef(t) =>
        val l = Try(value.asInstanceOf[NonEmptyChain[Any]].toNonEmptyList.toList)
          .orElse(Try(value.asInstanceOf[NonEmptyList[Any]].toList))
          .orElse(Try(value.asInstanceOf[NonEmptyVector[Any]].toVector.toList)).get
        getTypeWrappedVal(l, ArrayRef(t), interfaceContext)
      case CustomTypeRef(name, params, scalaType) =>
        val typeName = if (interfaceContext) interfaceName(name) else objectName(name)
        imports.custom(scalaType, typeName).getOrElse(imports.lift(typeName)) |+|
          (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
      case UnknownTypeRef(unknown) => unknown
      case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
      case u @ UnionType(_, params, possibilities, _) =>
        val valueType = scala.reflect.runtime.currentMirror.classSymbol(value.getClass).toType
        val (customTypeName, customType) = possibilities.collectFirst {
          case CustomTypeRef(n, _, t) if t =:= valueType => (n, t)
        }.getOrElse(sys.error(s"Value $value of type $valueType is not a member of union $u"))
        val objName = objectName(customTypeName)
        imports.custom(customType, objName).getOrElse(imports.lift(objName)) |+|
          (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
      case OptionType(it) => value.asInstanceOf[Option[_]] match {
        case Some(innerValue) => imports.fptsOption("some(") |+| getTypeWrappedVal(innerValue, it, interfaceContext) |+| ")"
        case None => imports.fptsOption("none")
      }
      case EitherType(lt, rt) =>
        Try(value.asInstanceOf[Either[_, _]]).orElse(Try(value.asInstanceOf[scalaz.\/[_, _]].toEither)).get match {
          case Left(v) => imports.fptsEither("left(") |+| getTypeWrappedVal(v, lt, interfaceContext) |+| ")"
          case Right(v) => imports.fptsEither("right(") |+| getTypeWrappedVal(v, rt, interfaceContext) |+| ")"
        }
      case TheseType(lt, rt) =>
        Try(value.asInstanceOf[cats.data.Ior[_, _]].unwrap)
          .orElse(Try(value.asInstanceOf[scalaz.\&/[_, _]]
            .fold(a => Left(Left(a)), b => Left(Right(b)), (a, b) => Right((a, b)))))
          .get match {
            case Left(Left(lv)) =>
              imports.fptsThese("left(") |+| getTypeWrappedVal(lv, lt, interfaceContext) |+| ")"

            case Left(Right(rv)) =>
              imports.fptsThese("right(") |+| getTypeWrappedVal(rv, rt, interfaceContext) |+| ")"

            case Right((lv, rv)) =>
              imports.fptsThese("both(") |+| getTypeWrappedVal(lv, lt, interfaceContext) |+| ", " |+|
                getTypeWrappedVal(rv, rt, interfaceContext) |+| ")"
          }
      case MapType(kt, vt) =>
        val m = value.asInstanceOf[Map[_, _]]
        if (m.isEmpty) "{}" else m.join("{ ", ", ", " }") { case (k, v) =>
          getTypeWrappedVal(k, kt, interfaceContext) |+| ": " |+| getTypeWrappedVal(v, vt, interfaceContext)
        }
      case TupleType(types) =>
        value.asInstanceOf[Product].productIterator.toList.zip(types.toList)
          .joinArray { case (v, t) => getTypeWrappedVal(v, t, interfaceContext) }
      case NullRef => "null"
      case UndefinedRef => "undefined"
    }
  }
}
