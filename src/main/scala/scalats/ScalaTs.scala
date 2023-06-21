package sts

import cats.{Eval, Monoid}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import io.circe.Json
import java.io.{File, PrintStream}
import scala.annotation.experimental
import scala.quoted.*
import scala.util.Using
import scala.util.chaining.*

private[sts] case class TypeParam[Name]()

case class ReferencedTypes(types: Map[TypeName, Set[TypeName]])

object ReferencedTypes {
  val empty = ReferencedTypes(Map.empty)

  given monoid: Monoid[ReferencedTypes] = Monoid.instance(empty, (a, b) => ReferencedTypes(a.types |+| b.types))
}

private case class ScalaTs(
  customType: Expr[ScalaTs.CustomType],
  imports: Expr[TsImports.available],
)(using override val ctx: Quotes)
extends ReflectionUtils
with TypeParser {
  import ctx.reflect.*
  import ScalaTs.*

  // Full type name, including package, excluding type parameters
  // e.g. `fullTypeName("foo.bar.Baz[Quux]")` => `"foo.bar.Baz"`
  private def fullTypeName(tpe: TypeRepr): String =
    tpe.show.split('[').head

  // Base type name, excluding type paramters
  // e.g. `baseTypeName("foo.bar.Baz[Quux]")` => `"Baz"`
  private def baseTypeName(tpe: TypeRepr): String =
    fullTypeName(tpe).split('.').last

  private def ordInstance[A: Type]: Expr[Generated] = {
    lazy val err = report.errorAndAbort(s"`Ord` instance requested for ${Type.show[A]} but not found")
    val typeRepr = TypeRepr.of[A]

    parseType(typeRepr) match {
      case T.String => '{ $imports.fptsString("Ord", Some("stringOrd")) }
      case T.Boolean => '{ $imports.fptsBoolean("Ord", Some("boolOrd")) }
      case T.Number => '{ $imports.fptsNumber("Ord", Some("numberOrd")) }
      case t: T.Eval[a] => ordInstance[a](using t.a)
      case t: T.List[a] => '{ $imports.fptsReadonlyArray($imports.lift("getOrd(") |+| ${ ordInstance[a](using t.a) } |+| ")") }
      case t: T.Option[a] => '{ $imports.fptsOption($imports.lift("Ord(") |+| ${ ordInstance[a](using t.a) } |+| ")") }

      case _: T.Unknown[_] =>
        Mirror(typeRepr) match {
          case Some(m) if m.mirrorType == MirrorType.Sum =>
            '{ $imports.custom(TypeName(${ Expr(fullTypeName(typeRepr)) }), unionOrdName(${ Expr(baseTypeName(typeRepr)) })) }

          case _ =>
            err
        }

      case (
        (_: T.TypeParam[_])
        | T.Json
        | T.BigNumber
        | T.LocalDate
        | T.DateTime
        | (_: T.Set[_])
        | (_: T.NonEmptyList[_])
        | (_: T.Either[_, _])
        | (_: T.Ior[_, _])
        | (_: T.Map[_, _])
        | T.EmptyTuple
        | (_: T.TupleRec[_, _])
        | (_: T.TupleN[_])
      ) =>
        err
    }
  }

  private def mkCodecName(tpe: TypeRepr): String =
    if (tpe.show != tpe.dealias.show)
      maybeDecap(baseTypeName(tpe)) + "C"
    else
      Mirror(tpe).map(_.mirrorType) match {
        case Some(MirrorType.Sum) => baseTypeName(tpe) + "CU"
        case Some(MirrorType.Product) | None => maybeDecap(baseTypeName(tpe)) + "C"
      }

  private object NumberType {
    def unapply[A](t: Type[A]): Boolean =
      t match {
        case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => true
        case _ => false
      }
  }

  @experimental
  private def tsRecordKeyType[A: Type](state: GenerateState): Expr[Generated] =
    TypeRepr.of[A].asType match {
      case NumberType() => '{ $imports.iotsNumberFromString }
      case _ => '{ ${ generate[A](state) }.foldMap(_._2) }
    }

  private def tsValue(typeRepr: TypeRepr, value: Expr[Any]): Expr[Generated] =
    parseType(typeRepr) match {
      case T.TypeParam() => '{ sys.error("Encountered unexpected type parameter in `tsValue`, value: " + $value) }
      case T.Json => '{ $imports.lift($value.asInstanceOf[Json].noSpaces) }
      case T.Number | T.BigNumber | T.Boolean => '{ $imports.lift($value.toString) }
      case T.String | T.LocalDate | T.DateTime => '{ $imports.lift("`" + $value.toString + "`") }
      case t: T.Eval[a] => t.a.pipe(implicit a => tsValue(TypeRepr.of[a], '{ $value.asInstanceOf[Eval[a]].value }))
      case l: T.List[a] => l.a.pipe { implicit a =>
        val t = TypeRepr.of[a]
        '{ $imports.lift("[") |+| ${ l.toList }($value).intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| "]" }
      }
      case t: T.Set[a] => t.a.pipe { implicit a =>
        val t = TypeRepr.of[a]
        '{
          $imports.fptsReadonlySet(
            $imports.lift("fromReadonlyArray(") |+|
              ${ ordInstance[a] } |+|
              ")([" |+|
              $value.asInstanceOf[Set[a]].toList.intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+|
              "])"
          )
        }
      }
      case t: T.NonEmptyList[a] => t.a.pipe(implicit a => tsValue(TypeRepr.of[List[a]], '{ ${ t.toNel }($value).toList }))
      case t: T.Option[a] =>
        t.a.pipe(implicit a => '{
          $imports.fptsOption($value.asInstanceOf[Option[a]].fold($imports.lift("none"))(
            a => $imports.lift("some(") |+| ${ tsValue(TypeRepr.of[a], 'a) } |+| ")"))
        })
      case t: T.Either[l, r] =>
        t.l.pipe(implicit l => t.r.pipe(implicit r => '{
          $imports.fptsEither(${ t.toEither }($value).fold(
            l => $imports.lift("left(") |+| ${ tsValue(TypeRepr.of[l], 'l) } |+| ")",
            r => $imports.lift("right(") |+| ${ tsValue(TypeRepr.of[r], 'r) } |+| ")",
          ))
        }))
      case t: T.Ior[l, r] =>
        t.l.pipe(implicit l => t.r.pipe { implicit r =>
          val (lt, rt) = (TypeRepr.of[l], TypeRepr.of[r])
          '{
            $imports.fptsThese(${ t.toIor }($value).fold(
              l => $imports.lift("left(") |+| ${ tsValue(lt, 'l) } |+| ")",
              r => $imports.lift("right(") |+| ${ tsValue(rt, 'r) } |+| ")",
              (l, r) => $imports.lift("both(") |+| ${ tsValue(lt, 'l) } |+| ", " |+| ${ tsValue(rt, 'r) } |+| ")",
            ))
          }
        })
      case t: T.Map[k, v] =>
        t.k.pipe(implicit k => t.v.pipe(implicit v => '{
          $value.asInstanceOf[Map[k, v]].pipe(m =>
            if (m.isEmpty) $imports.lift("{}")
            else $imports.lift("{ ") |+|
              m.toList.intercalateMap($imports.lift(",")) { case (k, v) =>
                ${ tsValue(TypeRepr.of[k], 'k) } |+| ": " |+| ${ tsValue(TypeRepr.of[v], 'v) }
              } |+|
              " }"
          )
        }))
      case T.EmptyTuple => '{ $imports.lift("[]") }
      case t: T.TupleRec[h, t] =>
        def unroll[T <: Tuple: Type](v: Expr[Any]): List[Expr[Generated]] =
          Type.of[T] match {
            case '[*:[h, t]] =>
              val t = '{ $v.asInstanceOf[*:[h, t]] }
              tsValue(TypeRepr.of[h], '{ $t.head }) :: unroll[t]('{ $t.tail })
            case '[EmptyTuple] => Nil
          }
        val gen = t.h.pipe(implicit h => t.t.pipe(implicit t => unroll[h *: t](value)))
        '{ $imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(gen) }) |+| "]" }
      case _: T.TupleN[_] =>
        val el = (i: Int) => '{ $value.asInstanceOf[Product].productElement(${ Expr(i) }) }
        val gen = typeRepr.typeArgs.zipWithIndex.map { case (t, i) => tsValue(t, el(i)) }
        '{ $imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(gen) }) |+| "]" }
      case t: T.Unknown[a] =>
        t.a.pipe(implicit a => '{
          $imports.custom(
            TypeName(${ Expr(fullTypeName(TypeRepr.of[a])) }),
            maybeDecap(clsNme($value))
          )
        })
    }

  private def valMembers(typeRepr: TypeRepr): List[Symbol] = {
    val typeSym = typeRepr.typeSymbol
    (typeSym.fieldMembers ++ typeSym.methodMembers).filter(s =>
      s.isValDef &&
        !s.isClassConstructor &&
        s.name != "toString" &&
        !(
          s.privateWithin.nonEmpty ||
          s.protectedWithin.nonEmpty ||
          s.flags.is(Flags.Private) ||
          s.flags.is(Flags.PrivateLocal) ||
          s.flags.is(Flags.Protected)
        )
    ).sortBy(_.name)
  }

  private def valDefType(typeRepr: TypeRepr, sym: Symbol): TypeRepr = {
    def unAnd(tpe: TypeRepr): TypeRepr = tpe match {
      case AndType(t, _) => unAnd(t)
      case AppliedType(t, ts) => AppliedType(t, ts.map(unAnd))
      case t => t
    }

    sym.tree match {
      case ValDef(_, _, Some(term)) => unAnd(term.tpe)
      case _ => report.errorAndAbort(s"Failed to get type of ${typeRepr.show}#${sym.name}")
    }
  }

  @experimental
  private def generateEnum[A: Type](mirror: Mirror, state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val valueType = Expr(baseTypeName(typeRepr))

    def parseMembers(m: Mirror): (Boolean, List[String], List[String], List[String], List[Expr[List[(Option[TypeName], Generated)]]]) = {
      given mb: Monoid[Boolean] = Monoid.instance(true, _ && _)

      m.mirrorType match {
        case MirrorType.Sum => m.types.toList.zipWithIndex.foldMap { case (t, i) =>
          t.asType match {
            case '[t] =>
              val name = m.labels(i)
              val nameExpr = Expr(name)
              val constName = maybeDecap(name)
              val constNameExpr = Expr(constName)
              val codecName = constName + "C"
              val codecNameExpr = Expr(codecName)
              val valueTypeExpr = Expr(cap(constName))
              val taggedCodecName = constName + "TaggedC"
              val taggedCodecNameExpr = Expr(taggedCodecName)
              val taggedValueTypeExpr = Expr(cap(taggedCodecName).stripSuffix("C"))

              Expr.summon[ValueOf[t]] match {
                case Some(v) =>
                  val members = valMembers(t)
                  val memberVals = Expr.ofList(members.map(s =>
                    '{
                      $imports.lift("  " + ${ Expr(s.name) } + ": ") |+|
                        ${ tsValue(valDefType(t, s), Select('{ $v.value }.asTerm, s).asExpr) }
                    }
                  ))
                  val codec = '{
                    // Const with all values
                    $imports.lift("export const " + $constNameExpr + " = {\n") |+|
                      ("  _tag: `" + $nameExpr + "`,\n") |+|
                      intercalate($imports.lift(",\n"))($memberVals) |+|
                      "\n} as const;\n\n" |+|
                      // Const with only `_tag` value
                      ("export const " + $taggedCodecNameExpr + " = ") |+|
                      $imports.iotsTypeFunction(
                        $imports.lift("{ _tag: ") |+|
                          $imports.iotsLiteral("`" + $nameExpr + "`") |+|
                          " }"
                      ) |+|
                      ";\n" |+|
                      // Tagged codec type
                      ("export type " + cap($taggedCodecNameExpr) + " = typeof " + $taggedCodecNameExpr + ";\n") |+|
                      // Tagged value type
                      ("export type " + $taggedValueTypeExpr + " = ") |+|
                      $imports.iotsTypeOf(cap($taggedCodecNameExpr)) |+|
                      ";\n" |+|
                      // Full value type
                      ("export type " + $valueTypeExpr + " = " +
                        $taggedValueTypeExpr + " & typeof " + $constNameExpr + ";\n") |+|
                      // Full codec type
                      ("export const " + $codecNameExpr + " = ") |+|
                      $imports.fptsPipe(
                        $imports.lift($taggedCodecNameExpr + ", c => new ") |+|
                          $imports.iotsTypeType |+|
                          ("<" + $valueTypeExpr + ", " + $taggedValueTypeExpr + ">(\n") |+|
                          ${ Expr("  `" + baseTypeName(typeRepr) + " " + name + "`,\n") } |+|
                          ("  (u: unknown): u is " + $valueTypeExpr + " => ") |+| $imports.fptsEither("isRight(c.decode(u)),\n") |+|
                          ("  (u: unknown): ") |+|
                          $imports.fptsEither("Either<") |+| $imports.iotsErrors |+| (", " + $valueTypeExpr + "> => ") |+|
                          $imports.fptsPipe(
                            $imports.lift("c.decode(u), ") |+|
                            $imports.fptsEither("map(x => ({ ...x, ..." + $constNameExpr + " }))")
                          ) |+|
                          ",\n" |+|
                          ("  (x: " + $valueTypeExpr + "): " + $taggedValueTypeExpr + " => ({ ...x, _tag: `" + $nameExpr + "` }),\n") |+|
                          ")"
                      ) |+|
                      ";"
                  }

                  (true, List(name), List(constName), List(codecName), List('{ List((Some(TypeName(${ Expr(fullTypeName(t)) })), $codec)) }))

                case None =>
                  Mirror(t).fold((false, Nil, Nil, Nil, Nil))(parseMembers)
              }
          }
        }
        case MirrorType.Product =>
          val name = baseTypeName(m.mirroredType)
          val codecName = mkCodecName(m.mirroredType)
          m.mirroredType.asType match {
            case '[t] => (false, List(name), Nil, List(codecName), List(generateTopLevel[t](true)))
          }
      }
    }

    val (allConstant, memberNames, memberConstNames, memberCodecNames, memberCodecs) = parseMembers(mirror)
    val allMemberCodecs =
      if (state.typeParamNames.isEmpty) memberCodecNames
      else memberCodecNames.map(n => n + "(" + state.typeParamNames.mkString(", ") + ")")
    val allMemberCodecsArr = '{ "[" + ${ Expr(allMemberCodecs.mkString(", ")) } + "]" }
    val allNamesConstName = '{ "all" + $valueType + "Names" }
    lazy val ordInst = '{
      $imports.lift("export const " + decap($valueType) + "Ord: ") |+|
        $imports.fptsOrd("Ord<" + $valueType + "U> = ") |+|
        $imports.fptsPipe(
          $imports.fptsString("Ord", Some("stringOrd")) |+|
            ", " |+|
            $imports.fptsOrd("contramap(x => x._tag)")
        ) |+|
        ";\n"
    }

    '{
      ${ Expr.ofList(memberCodecs) }.flatten |+| List((
        Some(TypeName(${ Expr(fullTypeName(typeRepr)) })),
        $imports.lift("export const all" + $valueType + "C = ") |+|
        ${ state.typeParamsTypes } |+|
        ${ state.typeParamsFnArgs } |+|
        (if (${ Expr(state.typeParamNames.nonEmpty) }) " => " else "") |+|
        ($allMemberCodecsArr + " as const;\n") |+|
        ("export const " + $allNamesConstName + " = [" +
          ${ Expr(memberNames.map(s => s"`$s`").mkString(", ")) } + "] as const;\n") |+|
        ("export type " + $valueType + "Name = (typeof " + $allNamesConstName + ")[number];\n") |+|
        "\n" |+|
        ${ state.wrapCodec(
          if (allMemberCodecs.length == 1) '{ $imports.lift(${ Expr(allMemberCodecs.head) }) }
          else '{ $imports.iotsUnion($allMemberCodecsArr) }
        ) } |+|
        "\n" |+|
        (if (${ Expr(state.typeParamNames.isEmpty) }) $ordInst else Generated.empty) |+|
        (
          if (${ Expr(allConstant) })
            $imports.lift("export const all" + $valueType + " = [" +
              ${ Expr(memberConstNames.mkString(", ")) } + "] as const;\n")
          else Generated.empty
        ) |+|
        (
          if (${ Expr(state.typeParamNames.isEmpty) })
            $imports.lift("export type " + $valueType + "Map<A> = { [K in " + $valueType + "Name]: A };\n")
          else
            Generated.empty
        )
      ))
    }
  }

  @experimental
  private def generateCaseClass[A: Type](mirror: Mirror, state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val typesAndLabels0 = mirror.types.toList.zip(mirror.labels).map {
      case (tpe, label) =>
        val res = tpe.asType match {
          case '[t] => '{ $imports.lift("  " + ${Expr(label)} + ": ") |+| ${ generate[t](state.copy(top = false)) }.foldMap(_._2) }
        }
        res
    }
    val typesAndLabels = Expr.ofList(
      if (state.inEnum)
        '{
          $imports.lift("  _tag: ") |+|
          $imports.iotsLiteral("`" + ${ Expr(baseTypeName(typeRepr)) } + "`")
        } :: typesAndLabels0
      else
        typesAndLabels0
    )

    val codec = state.wrapCodec('{
      $imports.iotsTypeFunction(
        $imports.lift("{\n") |+|
        intercalate($imports.lift(",\n"))($typesAndLabels) |+|
        "\n}"
      )
    })

    '{ List((Some(TypeName(${ Expr(fullTypeName(typeRepr)) })), $codec)) }
  }

  case class GenerateState(
    top: Boolean,
    inEnum: Boolean,
    typeParamNames: List[String],
    typeParamsTypes: Expr[Generated],
    typeParamsFnArgs: Expr[String],
    wrapCodec: Expr[Generated] => Expr[Generated],
  )

  @experimental
  def generate[A: Type](state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val handleTop = (gen: Expr[Generated]) =>
      if (state.top) '{ List((Some(TypeName(${ Expr(fullTypeName(typeRepr)) })), ${ state.wrapCodec(gen) })) }
      else '{ List((None, $gen)) }

    parseType(typeRepr) match {
      case t: T.TypeParam[a] =>
        TypeRepr.of[a](using t.a) match {
          case ConstantType(c) => c.value match {
            case s: String => handleTop('{ $imports.lift(${ Expr(s) }) })
          }
        }: @annotation.nowarn("msg=match may not be exhaustive")
      case T.Json => handleTop('{ $imports.iotsUnknown })
      case T.Number => handleTop('{ $imports.iotsNumber })
      case T.BigNumber => handleTop('{ $imports.iotsBigNumber })
      case T.Boolean => handleTop('{ $imports.iotsBoolean })
      case T.String => handleTop('{ $imports.iotsString })
      case T.LocalDate => handleTop('{ $imports.iotsLocalDate })
      case T.DateTime => handleTop('{ $imports.iotsDateTime })
      case t: T.Eval[a] => handleTop('{ ${ generate[a](state.copy(top = false))(using t.a) }.foldMap(_._2) })
      case t: T.List[a] => handleTop('{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false))(using t.a) }.foldMap(_._2)) })
      case t: T.Set[a] => handleTop('{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false))(using t.a) }.foldMap(_._2), ${ ordInstance[a](using t.a) }) })
      case t: T.NonEmptyList[a] => handleTop('{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false))(using t.a) }.foldMap(_._2)) })
      case t: T.Option[a] => handleTop('{ $imports.iotsOption(${ generate[a](state.copy(top = false))(using t.a) }.foldMap(_._2)) })
      case t: T.Either[l, r] => handleTop('{ $imports.iotsEither(${ generate[l](state.copy(top = false))(using t.l) }.foldMap(_._2), ${ generate[r](state.copy(top = false))(using t.r) }.foldMap(_._2)) })
      case t: T.Ior[l, r] => handleTop('{ $imports.iotsThese(${ generate[l](state.copy(top = false))(using t.l) }.foldMap(_._2), ${ generate[r](state.copy(top = false))(using t.r) }.foldMap(_._2)) })
      case t: T.Map[k, v] => handleTop('{ $imports.iotsRecord(${ tsRecordKeyType[k](state.copy(top = false))(using t.k) }, ${ generate[v](state.copy(top = false))(using t.v) }.foldMap(_._2)) })
      case T.EmptyTuple =>
        val cond = Expr("Array.isArray(u) && u.length === 0")
        handleTop('{
          $imports.lift("new ") |+| $imports.iotsTypeType |+| "<[]>(\n" |+|
          " \"EmptyTuple\",\n" |+|
          "  (u: unknown): u is [] => " |+| $cond |+| ",\n" |+|
          "  (u: unknown, context: " |+| $imports.iotsContext |+| ") =>\n" |+|
          ("    " |+| $cond) |+|
            " ? " |+| $imports.fptsEither("right([])") |+|
            " : " |+| $imports.fptsEither("left([{ context, value: u, message: \"Expected empty tuple\" }])") |+|
            ",\n" |+|
          "  (a: []): [] => a,\n"
        })
      case t: T.TupleRec[h, t] =>
        def unroll[T <: Tuple: Type]: List[Expr[List[(Option[TypeName], Generated)]]] =
          Type.of[T] match {
            case '[*:[h, t]] => generate[h](state.copy(top = false)) :: unroll[t]
            case '[EmptyTuple] => Nil
          }
        val gen = t.h.pipe(implicit h => t.t.pipe(implicit t => unroll[h *: t]))
        handleTop('{ $imports.iotsTuple($imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(gen) }.map(_.foldMap(_._2))) |+| "]") })
      case _: T.TupleN[_] =>
        val gen = typeRepr.typeArgs.map(_.asType match { case '[t] => generate[t](state.copy(top = false)) })
        handleTop('{ $imports.iotsTuple($imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(gen) }.map(_.foldMap(_._2))) |+| "]") })
      case _: T.Unknown[_] =>
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
              val tpeParamVals = Expr.ofList(params.map(_.asType match {
                case '[t] => '{ ${ generate[t](state.copy(top = false)) }.foldMap(_._2) }
              }))
              '{ $imports.lift("(") |+| intercalate($imports.lift(", "))($tpeParamVals) |+| ")" }

            case _ =>
              '{ Generated.empty }
          }

          '{
            List((None, $customType(${ Expr(typeRepr.show) })
              .getOrElse($imports.custom(
                TypeName(${ Expr(fullTypeName(typeRepr)) }),
                ${ Expr(mkCodecName(typeRepr)) }
              ) |+| $typeParams)))
          }
        }
    }
  }

  private val typeParamTypes = IndexedSeq(
    TypeRepr.of[TypeParam["A1"]],
    TypeRepr.of[TypeParam["A2"]],
    TypeRepr.of[TypeParam["A3"]],
    TypeRepr.of[TypeParam["A4"]],
    TypeRepr.of[TypeParam["A5"]],
    TypeRepr.of[TypeParam["A6"]],
    TypeRepr.of[TypeParam["A7"]],
    TypeRepr.of[TypeParam["A8"]],
    TypeRepr.of[TypeParam["A9"]],
    TypeRepr.of[TypeParam["A10"]],
    TypeRepr.of[TypeParam["A11"]],
    TypeRepr.of[TypeParam["A12"]],
    TypeRepr.of[TypeParam["A13"]],
    TypeRepr.of[TypeParam["A14"]],
    TypeRepr.of[TypeParam["A15"]],
    TypeRepr.of[TypeParam["A16"]],
    TypeRepr.of[TypeParam["A17"]],
    TypeRepr.of[TypeParam["A18"]],
    TypeRepr.of[TypeParam["A19"]],
    TypeRepr.of[TypeParam["A20"]],
    TypeRepr.of[TypeParam["A21"]],
    TypeRepr.of[TypeParam["A22"]],
  )

  @experimental
  def generateTopLevel[A: Type](inEnum: Boolean): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val (typeParamNames, typeParams, fnArgs, genCodec) = typeRepr match {
      case AppliedType(tpe, params) if params.nonEmpty =>
        val tps = params.zipWithIndex.map { case (_, i) => typeParamTypes(i) }
        val tpNames = tps.map(_.asType match {
          case '[TypeParam[a]] =>
            TypeRepr.of[a] match { case ConstantType(c) => c.value match { case s: String => s } }
        }): @annotation.nowarn("msg=match may not be exhaustive")
        val tpTypes = Expr.ofList(tpNames.map(t => '{ $imports.lift(${ Expr(t) } + " extends ") |+| $imports.iotsMixed }))
        val tpVals = Expr.ofList(tpNames.map(t => '{ ${ Expr(t) } + ": " + ${ Expr(t) } }))
        val tpsCode = '{
          $imports.lift("<") |+|
            intercalate($imports.lift(", "))($tpTypes) |+|
            ">"
        }
        val fnArgsCode = '{ $tpVals.mkString("(", ", ", ")") }
        val codec = AppliedType(tpe, tps).asType match { case '[t] => generate[t] }
        (tpNames, tpsCode, fnArgsCode, codec)

      case _ =>
        (Nil, '{ Generated.empty }, '{""}, generate[A])
    }

    val codecName = mkCodecName(typeRepr)
    val codecNameExpr = Expr(codecName)
    val codecType = Expr(cap(codecName))
    val valueType = Expr(cap(codecName).replaceAll("C(U?)$", "$1"))
    val className = Expr(codecName + "C")
    val joinedTPNames = Expr(typeParamNames.mkString(", "))
    val genState = GenerateState(true, inEnum, typeParamNames, typeParams, fnArgs, _)

    if (typeParamNames.nonEmpty)
      genCodec(genState(codec => '{
        $imports.lift("export class " + $className) |+|
          $typeParams |+|
          " { codec = " |+|
          $fnArgs |+|
          " => " |+|
          $codec |+|
          "}\n" |+|
          ("export const " + $codecNameExpr + " = ") |+|
          $typeParams |+|
          $fnArgs |+|
          (" => new " + $codecNameExpr + "C<" + $joinedTPNames + ">().codec(" + $joinedTPNames + ");\n") |+|
          ("export type " + $codecType) |+|
          $typeParams |+|
          (" = ReturnType<" + $className + "<" + $joinedTPNames + ">[\"codec\"]>;\n") |+|
          ("export type " + $valueType + "<" + $joinedTPNames + "> = ") |+|
          $imports.iotsTypeOf(
            $imports.lift($codecType + "<") |+|
              ${ Expr(typeParamNames) }.intercalateMap($imports.lift(", "))(
                t => $imports.iotsTypeType |+| ("<" + t + ">")) |+|
              ">"
          ) |+|
          ";"
      }))
    else
      genCodec(genState(codec => '{
        $imports.lift("export const " + $codecNameExpr + " = ") |+|
          $codec |+|
          ";\n" |+|
          ("export type " + $codecType + " = typeof " + $codecNameExpr + ";\n") |+|
          ("export type " + $valueType + " = ") |+|
          $imports.iotsTypeOf($codecType) |+|
          ";\n"
      }))
  }

  @experimental
  private def referencedTypeNamesEnum[A: Type](mirror: Mirror, currType: String): Map[String, Set[String]] =
    mirror.types.foldMap { t =>
      val typeName = fullTypeName(t)
      Map(currType -> Set(typeName)) |+| (t.asType match {
        case '[t] => Expr.summon[ValueOf[t]] match {
          case Some(_) => valMembers(t).foldMap(valDefType(t, _).asType match { case '[t] => referencedTypeNames[t](false, typeName) })
          case None => referencedTypeNames[t](true, typeName)
        }
      })
    }

  @experimental
  private def referencedTypeNamesCaseClass[A: Type](mirror: Mirror, currType: String): Map[String, Set[String]] =
    mirror.types.foldMap(_.asType match { case '[t] => referencedTypeNames[t](false, currType) })

  @experimental
  private def referencedTypeNames[A: Type](top: Boolean, currType: String): Map[String, Set[String]] = {
    val typeRepr = TypeRepr.of[A]
    parseType(typeRepr) match {
      case T.TypeParam() => Map.empty
      case T.Json => Map.empty
      case T.Number => Map.empty
      case T.BigNumber => Map.empty
      case T.Boolean => Map.empty
      case T.String => Map.empty
      case T.LocalDate => Map.empty
      case T.DateTime => Map.empty
      case t: T.Eval[a] => referencedTypeNames[a](false, currType)(using t.a)
      case t: T.List[a] => referencedTypeNames[a](false, currType)(using t.a)
      case t: T.Set[a] => referencedTypeNames[a](false, currType)(using t.a)
      case t: T.NonEmptyList[a] => referencedTypeNames[a](false, currType)(using t.a)
      case t: T.Option[a] => referencedTypeNames[a](false, currType)(using t.a)
      case t: T.Either[l, r] => referencedTypeNames[l](false, currType)(using t.l) |+| referencedTypeNames[r](false, currType)(using t.r)
      case t: T.Ior[l, r] => referencedTypeNames[l](false, currType)(using t.l) |+| referencedTypeNames[r](false, currType)(using t.r)
      case t: T.Map[k, v] => referencedTypeNames[k](false, currType)(using t.k) |+| referencedTypeNames[v](false, currType)(using t.v)
      case T.EmptyTuple => Map.empty
      case t: T.TupleRec[h, t] => referencedTypeNames[h](false, currType)(using t.h) |+| referencedTypeNames[t](false, currType)(using t.t)
      case _: T.TupleN[_] => typeRepr.typeArgs.foldMap(_.asType match { case '[t] => referencedTypeNames[t](false, currType) })
      case _: T.Unknown[_] =>
        if (top) {
          Mirror(typeRepr) match {
            case Some(m) =>
              m.mirrorType match {
                case MirrorType.Sum => referencedTypeNamesEnum[A](m, currType)
                case MirrorType.Product => referencedTypeNamesCaseClass[A](m, currType)
              }

            case None =>
              Map.empty
          }
        } else {
          Map(currType -> Set(fullTypeName(typeRepr))) |+| typeRepr.typeArgs.foldMap(_.asType match {
            case '[t] => referencedTypeNames[t](false, currType)
          })
        }
    }
  }

  @experimental
  def referencedTypes[A: Type]: Expr[ReferencedTypes] =
    '{
      ReferencedTypes(
        ${ Expr(referencedTypeNames[A](true, fullTypeName(TypeRepr.of[A]))) }
          .map { case (t, ts) => (TypeName(t), ts.map(TypeName(_))) }
      )
    }
}

object ScalaTs {
  def intercalate(glue: Generated)(xs: List[Generated]): Generated =
    xs match {
      case Nil => Generated.empty
      case h :: t => t.foldLeft(h)((acc, x) => acc |+| glue |+| x)
    }

  def clsNme(a: Any): String = a.getClass.getSimpleName.stripSuffix("$")

  def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  private val allCapsRx = """^[A-Z0-9_]+$""".r
  def maybeDecap(s: String): String =
    s match {
      case allCapsRx() => s
      case _ => decap(s)
    }
  def unionOrdName(s: String): String = decap(s) + "Ord"

  trait CustomType {
    def apply(name: String): Option[Generated]
  }

  object CustomType {
    val none = new CustomType {
      def apply(name: String) = None
    }
  }

  @experimental
  private def generateImpl[A: Type](
    customType: Expr[CustomType],
    imports: Expr[TsImports.available],
  )(using ctx: Quotes): Expr[(List[(Option[TypeName], Generated)], ReferencedTypes)] = {
    val sts = new ScalaTs(customType, imports)
    Expr.ofTuple((sts.generateTopLevel[A](false), sts.referencedTypes[A]))
  }

  inline def generate[A](
    inline customType: CustomType,
    inline imports: TsImports.available,
  ): (List[(Option[TypeName], Generated)], ReferencedTypes) =
    ${ generateImpl[A]('customType, 'imports) }

  @experimental
  private def referenceCodeImpl[A: Type](
    customType: Expr[CustomType],
    imports: Expr[TsImports.available],
  )(using ctx: Quotes): Expr[Generated] = {
    val sts = new ScalaTs(customType, imports)
    val gen = sts.generate[A](sts.GenerateState(false, false, Nil, '{ Generated.empty }, '{""}, identity))
    '{ $gen.foldMap(_._2) }
  }

  inline def referenceCode[A](
    inline customType: CustomType,
    inline imports: TsImports.available,
  ): Generated =
    ${ referenceCodeImpl[A]('customType, 'imports) }

  private def sortTypes(types: List[(Option[TypeName], Generated)], refs: ReferencedTypes): List[Generated] = {
    val all = types.flatMap { case (typeName, generated) => typeName.map((_, generated)) }.toMap

    def addType(
      acc: Eval[(List[Generated], Set[TypeName])],
      typeName: TypeName,
      generated: Generated,
    ): Eval[(List[Generated], Set[TypeName])] = {
      acc.flatMap { case (accGen, accSkip) =>
        if (accSkip.contains(typeName))
          Eval.now((accGen, accSkip))
        else
          refs.types.get(typeName) match {
            case Some(types) =>
              types.foldLeft(Eval.now((accGen, accSkip + typeName)))(
                (acc, ref) => all.get(ref).fold(acc)(addType(acc, ref, _))
              ).map { case (x, y) => (x :+ generated, y) }
            case None =>
              Eval.now((accGen :+ generated, accSkip + typeName))
          }
      }
    }

    types.foldLeft(Eval.now(List.empty[Generated], Set.empty[TypeName])) {
      case (acc, (Some(typeName), generated)) => addType(acc, typeName, generated)
      case (acc, (None, generated)) => acc.map { case (accGen, accSkip) => (accGen :+ generated, accSkip) }
    }.value._1
  }

  private def withFileWriter[A](file: File)(f: PrintStream => A): A = {
    new File(file.getParent).mkdirs
    Using.resource(new PrintStream(file))(f)
  }

  private def mkAllTypesByFile(allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Map[String, Set[TypeName]] =
    allGenerated.foldMap { case (f, ts) => Map(f.toString -> ts._1.flatMap(_._1).toSet) }

  def resolve(
    currFile: File,
    generated: Generated,
    allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))],
  ): (Map[TsImport.Resolved, TsImport], String) = {
    val allTypesByFile = mkAllTypesByFile(allGenerated)
    val Generated(imports, code) = generated
    imports.resolve(currFile.toString, allTypesByFile, code)
  }

  def write(allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Unit = {
    val allTypesByFile = mkAllTypesByFile(allGenerated)
    val all = allGenerated.foldMap { case (file, l) => Map(file.toString -> l) }.map { case (file, (types, refs)) =>
      val (allImports, allCode) = sortTypes(types, refs).foldMap { case Generated(imports, code) =>
        val (updImports, updCode) = imports.resolve(file.toString, allTypesByFile, code)
        (updImports, updCode + "\n\n")
      }
      (file, allImports.map { case (loc, imprt) =>
        s"""import ${imprt.str} from "${loc.location}";"""
      }.mkString("\n") + "\n\n" + allCode)
    }
    all.foreach { case (file, code) => withFileWriter(new File(file))(_.print(code)) }
  }
}
