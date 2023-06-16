package scalats

import cats.{Eval, Monoid}
import cats.data.{Chain, Ior, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.syntax.foldable._
import cats.syntax.semigroup._
import io.circe.Json
import java.time.{Instant, LocalDate => JLocalDate, LocalDateTime, ZonedDateTime}
import org.joda.time.{DateTime, LocalDate}
import scala.collection.immutable.SortedSet
import scala.quoted._
import scala.util.chaining._

private case class ScalaTs(config: Expr[Config])(using override val ctx: Quotes) extends ReflectionUtils {
  import ctx.reflect._

  final val imports = '{ TsImports.available(${config}) }

  private def baseTypeName(tpe: TypeRepr): String =
    tpe.show.split('[').head.split('.').last

  private def ordInstance[A: Type]: Expr[TsImports.With[String]] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[String] => '{ $imports.fptsString("Ord", Some("stringOrd")) }
      case '[Boolean] => '{ $imports.fptsBoolean("Ord", Some("boolOrd")) }
      case NumberType() => '{ $imports.fptsNumber("Ord", Some("numberOrd")) }
      case '[Option[a]] => '{ $imports.fptsOption($imports.lift("Ord(") |+| ${ ordInstance[a] } |+| $imports.lift(")")) }
      case _ =>
        Mirror(typeRepr) match {
          case Some(m) if m.mirrorType == MirrorType.Sum =>
            '{ $imports.custom(TypeName(${ Expr(typeRepr.show) }), ScalaTs.unionOrdName(${ Expr(baseTypeName(typeRepr)) })) }

          case _ =>
            report.errorAndAbort(s"`Ord` instance requested for ${typeRepr.show} but not found")
        }
    }
  }

  private def mkCodecName(tpe: TypeRepr): String =
    ScalaTs.decapitalize(baseTypeName(tpe)) ++ "C"

  private object NumberType {
    def unapply[A](t: Type[A]): Boolean =
      t match {
        case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => true
        case _ => false
      }
  }

  private def tsRecordKeyType[A: Type]: Expr[TsImports.With[String]] =
    TypeRepr.of[A].asType match {
      case '[String] => '{ $imports.iotsString }
      case NumberType() => '{ $imports.iotsNumberFromString }
      case _ => report.errorAndAbort(s"Unknown record key type: `${Type.show[A]}`")
    }

  private def tsValue(expr: Expr[Any]): Expr[TsImports.With[String]] =
    expr match {
      case '{ ${x}: Json } => '{ $imports.lift($x.noSpaces) }
      case '{ ${x}: (Byte | Short | Int | Long | Double | Float | BigDecimal | Boolean) } => '{ $imports.lift($x.toString) }
      case '{ ${x}: (String | LocalDate | JLocalDate | DateTime | LocalDateTime | ZonedDateTime | Instant) } => '{ $imports.lift("`" ++ $x.toString ++ "`") }
      case '{ ${x}: Eval[_] } => tsValue('{ $x.value })
      case '{ ${x}: Chain[_] } =>
        '{ $imports.lift("[") |+| $x.intercalateMap($imports.lift(", "))(a => ${ tsValue('a) }) |+| $imports.lift("]") }
      case '{ ${x}: List[_] } =>
        '{ $imports.lift("[") |+| $x.intercalateMap($imports.lift(", "))(a => ${ tsValue('a) }) |+| $imports.lift("]") }
      case '{ ${x}: Vector[_] } =>
        '{ $imports.lift("[") |+| $x.intercalateMap($imports.lift(", "))(a => ${ tsValue('a) }) |+| $imports.lift("]") }
      case '{ ${x}: Seq[_] } =>
        '{ $imports.lift("[") |+| $x.intercalateMap($imports.lift(", "))(a => ${ tsValue('a) }) |+| $imports.lift("]") }
      case '{ ${x}: Set[a] } =>
        '{
          $imports.fptsReadonlySet(
            $imports.lift("fromReadonlyArray(") |+|
              ${ ordInstance[a] } |+|
              $imports.lift(")([") |+|
              $x.toList.intercalateMap($imports.lift(", "))(a => ${ tsValue('a) }) |+|
              $imports.lift("])")
          )
        }
      case '{ ${x}: NonEmptyChain[a] } => tsValue('{ $x.toChain })
      case '{ ${x}: NonEmptyList[_] } => tsValue('{ $x.toList })
      case '{ ${x}: NonEmptyVector[_] } => tsValue('{ $x.toVector })
      case '{ ${x}: scalaz.NonEmptyList[_] } => tsValue('{ $x.list.toList })
      case '{ ${x}: Option[_] } =>
        '{
          $imports.fptsOption($x.fold($imports.lift("none"))(
            _ => $imports.lift("some(") |+| ${ tsValue('{ $x.get }) } |+| $imports.lift(")")))
        }
      case '{ ${x}: Either[_, _] } =>
        '{
          $imports.fptsEither($x.fold(
            _ => $imports.lift("left(") |+| ${ tsValue('{ $x.swap.toOption.get }) } |+| $imports.lift(")"),
            _ => $imports.lift("right(") |+| ${ tsValue('{ $x.toOption.get }) } |+| $imports.lift(")"),
          ))
        }
      case '{ ${x}: scalaz.\/[_, _] } => tsValue('{ $x.toEither })
      case '{ ${x}: Ior[_, _] } =>
        '{
          $imports.fptsThese($x.fold(
            _ => $imports.lift("left(") |+| ${ tsValue('{ $x.left.get }) } |+| $imports.lift(")"),
            _ => $imports.lift("right(") |+| ${ tsValue('{ $x.right.get }) } |+| $imports.lift(")"),
            (_, _) => $imports.lift("both(") |+| ${ tsValue('{ $x.left.get }) } |+| $imports.lift(", ") |+| ${ tsValue('{ $x.right.get }) } |+| $imports.lift(")"),
          ))
        }
      case '{ ${x}: scalaz.\&/[_, _] } =>
        tsValue('{ $x.fold(_ => Ior.Left($x.a.get), _ => Ior.Right($x.b.get), (_, _) => Ior.Both($x.a.get, $x.b.get)) })
      case '{ ${x}: Map[_, _] } =>
        '{
          $imports.lift("{") |+|
            $x.toList.intercalateMap($imports.lift(", ")) { case (k, v) =>
              ${ tsValue('k) } |+| $imports.lift(": ") |+| ${ tsValue('v) }
            } |+|
            $imports.lift("}")
        }
      case '{ ${x}: t } =>
        '{ $imports.custom(TypeName(${ Expr(TypeRepr.of[t].widen.show) }), ScalaTs.decapitalize($x.toString)) }
    }

  private val excludeMethods = Set(
    "##",
    "hashCode",
    "notify",
    "notifyAll",
    "productArity",
    "productElementNames",
    "productIterator",
    "productPrefix",
    "toString",
    "wait",
  )

  def generateEnum[A: Type](mirror: Mirror, state: GenerateState): Expr[TsImports.With[String]] = {
    val typeRepr = TypeRepr.of[A]
    val valueType = Expr(baseTypeName(typeRepr))
    given mb: Monoid[Boolean] = Monoid.instance(false, _ || _)
    val (memberNames, memberCodecNames, memberCodecs, anyDynamic) = mirror.types.zipWithIndex.foldMap { case (t, i) =>
      t.asType match {
        case '[t] =>
          val typeSym = t.typeSymbol
          val tag = Expr(mirror.labels(i))
          val constName = '{ ScalaTs.decapitalize($tag) }
          val codecName = '{ $constName ++ "C" }
          val valueType = '{ $constName.capitalize }
          val taggedCodecName = '{ $constName ++ "TaggedC" }
          val taggedValueType = '{ $taggedCodecName.capitalize.stripSuffix("C") }
          val (codec, dynamic) = Expr.summon[ValueOf[t]] match {
            case Some(v) =>
              val members = (typeSym.fieldMembers ++ typeSym.methodMembers).filter(s =>
                !s.isClassConstructor &&
                  (s.isValDef || (s.isDefDef && s.signature.paramSigs.isEmpty)) &&
                  !(s.flags.is(Flags.Private) || s.flags.is(Flags.PrivateLocal) || s.flags.is(Flags.Protected)) &&
                  !excludeMethods.contains(s.name)
              ).sortBy(_.name)
              val memberVals = Expr.ofList(members.map(s =>
                '{
                  $imports.lift("  \"" ++ ${ Expr(s.name) } ++ "\": ") |+|
                    ${ tsValue(Select('{ $v.value }.asTerm, s).asExpr) }
                }
              ))
              val codec = '{
                // Const with all values
                $imports.lift("export const " ++ $constName ++ " = {\n") |+|
                  $imports.lift("  _tag: `" ++ $tag ++ "`,\n") |+|
                  $memberVals.intercalate($imports.lift(",\n")) |+|
                  $imports.lift("\n} as const;\n\n") |+|
                  // Const with only `_tag` value
                  $imports.lift("export const " ++ $taggedCodecName ++ " = ") |+|
                  $imports.iotsTypeFunction(
                    $imports.lift("{ _tag: ") |+|
                      $imports.iotsLiteral("`" ++ $tag ++ "`") |+|
                      $imports.lift(" }")
                  ) |+|
                  $imports.lift(";\n") |+|
                  // Tagged codec type
                  $imports.lift("export type " ++ $taggedCodecName.capitalize ++ " = typeof " ++ $taggedCodecName ++ ";\n") |+|
                  // Tagged value type
                  $imports.lift("export type " ++ $taggedValueType ++ " = ") |+|
                  $imports.iotsTypeOf($taggedCodecName.capitalize) |+|
                  $imports.lift(";\n") |+|
                  // Full value type
                  $imports.lift("export type " ++ $valueType ++ " = " ++
                    $taggedValueType ++ " & typeof " ++ $constName ++ ";\n") |+|
                  // Full codec type
                  $imports.lift("export const " ++ $codecName ++ " = ") |+|
                  $imports.fptsPipe(
                    $imports.lift($taggedCodecName ++ ", c => new ") |+|
                      $imports.iotsTypeType |+|
                      $imports.lift("<" ++ $valueType ++ ", " ++ $taggedValueType ++ ">(\n") |+|
                      $imports.lift("  `" ++ ${ Expr(baseTypeName(typeRepr)) } ++ " " ++ $tag ++ "`,\n") |+|
                      $imports.lift("  (u: unknown): u is " ++ $valueType ++ " => ") |+| $imports.fptsEither("isRight(c.decode(u)),\n") |+|
                      $imports.lift("  (u: unknown): ") |+|
                      $imports.fptsEither("Either<") |+| $imports.iotsErrors |+| $imports.lift(", " ++ $valueType ++ "> => ") |+|
                      $imports.fptsPipe(
                        $imports.lift("c.decode(u), ") |+|
                        $imports.fptsEither("map(x => ({ ...x, ..." ++ $constName ++ " }))")
                      ) |+|
                      $imports.lift(",\n") |+|
                      $imports.lift("  (x: " ++ $valueType ++ "): " ++ $taggedValueType ++ " => ({ ...x, _tag: `" ++ $tag ++ "` }),\n") |+|
                      $imports.lift(")")
                  ) |+|
                  $imports.lift(";")
              }
              (codec, false)

            case None =>
              (generateTopLevel[t](true), true)
          }
          (List(tag), List(codecName), List(codec), dynamic)
      }: @annotation.nowarn("msg=match may not be exhaustive")
    }

    val allMemberCodecs =
      if (state.typeParamNames.isEmpty) memberCodecNames
      else memberCodecNames.map(n => '{ $n ++ "(" ++ ${ Expr(state.typeParamNames.mkString(", ")) } ++ ")" })
    val allMemberCodecsArr = '{ "[" ++ ${ Expr.ofList(allMemberCodecs) }.mkString(", ") ++ "]" }
    val allNamesConstName = '{ "all" ++ $valueType ++ "Names" }
    lazy val ordInst = '{
      $imports.lift("export const " ++ ScalaTs.decapitalize($valueType) ++ "Ord: ") |+|
        $imports.fptsOrd("Ord<" ++ $valueType ++ "> = ") |+|
        $imports.fptsPipe(
          $imports.fptsString("Ord", Some("stringOrd")) |+|
            $imports.lift(", ") |+|
            $imports.fptsOrd("contramap(x => x._tag)")
        ) |+|
        $imports.lift(";\n")
    }

    '{
      ${ Expr.ofList(memberCodecs) }.intercalate($imports.lift("\n\n")) |+|
      $imports.lift("\n\n") |+|
      $imports.lift("export const all" ++ $valueType ++ "C = ") |+|
      ${ state.typeParamsTypes } |+|
      $imports.lift(${ state.typeParamsFnArgs }) |+|
      $imports.lift(if (${ Expr(state.typeParamNames.nonEmpty) }) " => " else "") |+|
      $imports.lift($allMemberCodecsArr ++ " as const;\n") |+|
      $imports.lift("export const " ++ $allNamesConstName ++ " = [" ++
        ${ Expr.ofList(memberNames) }.map(s => s"`$s`").mkString(", ") ++ "] as const;\n") |+|
      $imports.lift("export type " ++ $valueType ++ "Name = (typeof " ++ $allNamesConstName ++ ")[number];\n") |+|
      $imports.lift("\n") |+|
      ${ state.wrapCodec('{ $imports.iotsUnion($allMemberCodecsArr) }) } |+|
      $imports.lift("\n") |+|
      (if (${ Expr(state.typeParamNames.isEmpty) }) $ordInst else $imports.emptyStr) |+|
      (
        if (${ Expr(anyDynamic) }) $imports.emptyStr
        else $imports.lift("export const all" ++ $valueType ++ " = [" ++
          ${ Expr.ofList(memberNames) }.map(ScalaTs.decapitalize).mkString(", ") ++ "] as const;\n")
      ) |+|
      (
        if (${ Expr(state.typeParamNames.isEmpty) })
          $imports.lift("export type " ++ $valueType ++ "Map<A> = { [K in " ++ $valueType ++ "Name]: A };\n")
        else
          $imports.emptyStr
      )
    }
  }

  def generateCaseClass[A: Type](mirror: Mirror, state: GenerateState): Expr[TsImports.With[String]] = {
    val typesAndLabels0 = mirror.types.toList.zip(mirror.labels).map {
      case (tpe, label) =>
        val res = tpe.asType match {
          case '[t] => '{ $imports.lift("  " ++ ${Expr(label)} ++ ": ") |+| ${ generate[t](state.copy(top = false)) } }
        }: @annotation.nowarn("msg=match may not be exhaustive")
        res
    }
    val typesAndLabels = Expr.ofList(
      if (state.inEnum)
        '{
          $imports.lift("  _tag: ") |+|
          $imports.iotsLiteral("`" ++ ${ Expr(baseTypeName(TypeRepr.of[A])) } ++ "`")
        } :: typesAndLabels0
      else
        typesAndLabels0
    )

    state.wrapCodec('{
      $imports.iotsTypeFunction(
        $imports.lift("{\n") |+|
        $typesAndLabels.intercalate($imports.lift(",\n")) |+|
        $imports.lift("\n}")
      )
    })
  }

  // def generateCaseObject[A: Type](top: Boolean): GeneratedCode = ???

  type WrapCodec = Expr[TsImports.With[String]] => Expr[TsImports.With[String]]

  case class GenerateState(
    top: Boolean,
    inEnum: Boolean,
    typeParamNames: List[String],
    typeParamsTypes: Expr[TsImports.With[String]],
    typeParamsFnArgs: Expr[String],
    wrapCodec: WrapCodec,
  )

  def generate[A: Type](state: GenerateState): Expr[TsImports.With[String]] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[ScalaTs.TypeParam[a]] =>
        TypeRepr.of[a] match {
          case ConstantType(c) => c.value match { case s: String => '{ $imports.lift(${ Expr(s) }) } }
        }: @annotation.nowarn("msg=match may not be exhaustive")

      case '[Json] => '{ $imports.iotsUnknown }
      case NumberType() => '{ $imports.iotsNumber }
      case '[BigDecimal] => '{ $imports.iotsBigNumber }
      case '[Boolean] => '{ $imports.iotsBoolean }
      case '[String] => '{ $imports.iotsString }
      case '[LocalDate] => '{ $imports.iotsLocalDate }
      case '[JLocalDate] => '{ $imports.iotsLocalDate }
      case '[DateTime] => '{ $imports.iotsDateTime }
      case '[LocalDateTime] => '{ $imports.iotsDateTime }
      case '[ZonedDateTime] => '{ $imports.iotsDateTime }
      case '[Instant] => '{ $imports.iotsDateTime }
      case '[Eval[a]] => generate[a](state.copy(top = false))
      case '[Chain[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[List[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Vector[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Seq[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Set[a]] => '{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }, ${ ordInstance[a] }) }
      case '[SortedSet[a]] => '{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }, ${ ordInstance[a] }) }
      case '[NonEmptyChain[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[NonEmptyList[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[NonEmptyVector[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[scalaz.NonEmptyList[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Option[a]] => '{ $imports.iotsOption(${ generate[a](state.copy(top = false)) }) }
      case '[Either[l, r]] => '{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[scalaz.\/[l, r]] => '{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[Ior[l, r]] => '{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[scalaz.\&/[l, r]] => '{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[Map[k, v]] => '{ $imports.iotsRecord(${tsRecordKeyType[k]}, ${ generate[v](state.copy(top = false)) }) }
      case _ =>
        if (state.top) {
          Mirror(typeRepr) match {
            case Some(mirror) =>
              mirror.mirrorType match {
                case MirrorType.Sum => generateEnum[A](mirror, state)
                case MirrorType.Product => generateCaseClass[A](mirror, state)
              }

            case None =>
              report.errorAndAbort(s"Can't generate TS code for ${typeRepr.show}")
          }
        } else {
          val typeParams = typeRepr match {
            case AppliedType(_, params) =>
              @annotation.nowarn("msg=match may not be exhaustive")
              val tpeParamVals = Expr.ofList(params.map(_.asType match { case '[t] => generate[t](state.copy(top = false)) }))

              '{
                $imports.lift("(") |+|
                  ($tpeParamVals.foldMap { case (i, s) => (i, List(s)) }.pipe { case (i, l) => (i, l.mkString(", ")) }) |+|
                  $imports.lift(")")
              }

            case _ =>
              '{ $imports.emptyStr }
          }

          '{ $imports.custom(TypeName(${ Expr(typeRepr.show) }), ${ Expr(mkCodecName(typeRepr)) }) |+| $typeParams }
        }
    }
  }

  private val typeParamTypes = IndexedSeq(
    TypeRepr.of[ScalaTs.TypeParam["A1"]],
    TypeRepr.of[ScalaTs.TypeParam["A2"]],
    TypeRepr.of[ScalaTs.TypeParam["A3"]],
    TypeRepr.of[ScalaTs.TypeParam["A4"]],
    TypeRepr.of[ScalaTs.TypeParam["A5"]],
    TypeRepr.of[ScalaTs.TypeParam["A6"]],
    TypeRepr.of[ScalaTs.TypeParam["A7"]],
    TypeRepr.of[ScalaTs.TypeParam["A8"]],
    TypeRepr.of[ScalaTs.TypeParam["A9"]],
    TypeRepr.of[ScalaTs.TypeParam["A10"]],
  )

  def generateTopLevel[A: Type](inEnum: Boolean): Expr[TsImports.With[String]] = {
    val typeRepr = TypeRepr.of[A]
    val (typeParamNames, typeParams, fnArgs, genCodec) = typeRepr match {
      case AppliedType(tpe, params) if params.nonEmpty =>
        val tps = params.zipWithIndex.map { case (_, i) => typeParamTypes(i) }
        val tpNames = tps.map(_.asType match {
          case '[ScalaTs.TypeParam[a]] =>
            TypeRepr.of[a] match { case ConstantType(c) => c.value match { case s: String => s } }
        }): @annotation.nowarn("msg=match may not be exhaustive")
        val tpTypes = Expr.ofList(tpNames.map(t => '{ $imports.lift(${ Expr(t) } ++ " extends ") |+| $imports.iotsMixed }))
        val tpVals = Expr.ofList(tpNames.map(t => '{ ${ Expr(t) } ++ ": " ++ ${ Expr(t) } }))
        val tpsCode = '{
          $imports.lift("<") |+|
            ($tpTypes.foldMap { case (i, s) => (i, List(s)) }.pipe { case (i, l) => (i, l.mkString(", ")) }) |+|
            $imports.lift(">")
        }
        val fnArgsCode = '{ $tpVals.mkString("(", ", ", ")") }
        @annotation.nowarn("msg=match may not be exhaustive")
        val codec = AppliedType(tpe, tps).asType match { case '[t] => generate[t] }
        (tpNames, tpsCode, fnArgsCode, codec)

      case _ =>
        (Nil, '{ $imports.emptyStr }, '{""}, generate[A])
    }

    val codecName0 = mkCodecName(typeRepr)
    val codecName = Expr(codecName0)
    val codecType = Expr(codecName0.capitalize)
    val valueType = Expr(codecName0.capitalize.stripSuffix("C"))
    val className = Expr(codecName0 ++ "C")
    val joinedTPNames = Expr(typeParamNames.mkString(", "))
    val genState = GenerateState(true, inEnum, typeParamNames, typeParams, fnArgs, _)

    if (typeParamNames.nonEmpty)
      genCodec(genState(codec => '{
        $imports.lift("export class " ++ $className) |+|
          $typeParams |+|
          $imports.lift(" { codec = ") |+|
          $imports.lift($fnArgs) |+|
          $imports.lift(" => ") |+|
          $codec |+|
          $imports.lift("}\n") |+|
          $imports.lift("export const " ++ $codecName ++ " = ") |+|
          $typeParams |+|
          $imports.lift($fnArgs) |+|
          $imports.lift(" => new " ++ $codecName ++ "C<" ++ $joinedTPNames ++ ">().codec(" ++ $joinedTPNames ++ ");\n") |+|
          $imports.lift("export type " ++ $codecType) |+|
          $typeParams |+|
          $imports.lift(" = ReturnType<" ++ $className ++ "<" ++ $joinedTPNames ++ ">[\"codec\"]>;\n") |+|
          $imports.lift("export type " ++ $valueType ++ "<" ++ $joinedTPNames ++ "> = ") |+|
          $imports.iotsTypeOf(
            $imports.lift($codecType ++ "<") |+|
              ${ Expr(typeParamNames) }.intercalateMap($imports.lift(", "))(
                t => $imports.iotsTypeType |+| $imports.lift("<" ++ t ++ ">")) |+|
              $imports.lift(">")
          ) |+|
          $imports.lift(";")
      }))
    else
      genCodec(genState(codec => '{
        $imports.lift("export const " ++ $codecName ++ " = ") |+|
          $codec |+|
          $imports.lift(";\n") |+|
          $imports.lift("export type " ++ $codecType ++ " = typeof " ++ $codecName ++ ";\n") |+|
          $imports.lift("export type " ++ $valueType ++ " = ") |+|
          $imports.iotsTypeOf($codecType) |+|
          $imports.lift(";\n")
      }))
  }
}

object ScalaTs {
  private[ScalaTs] case class TypeParam[Name]()

  def decapitalize(s: String): String = s.take(1).toLowerCase ++ s.drop(1)
  def unionOrdName(s: String): String = decapitalize(s) ++ "Ord"

  private def generateImpl[A: Type](config: Expr[Config])(using ctx: Quotes): Expr[TsImports.With[String]] =
    new ScalaTs(config).generateTopLevel[A](false)

  inline def generate[A](inline config: Config): TsImports.With[String] =
    ${ generateImpl[A]('config) }
}
