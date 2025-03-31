package scalats

import scala.quoted.*
import scala.util.chaining.*

/**
 * Parses scala types to produce instances of [[scalats.TsModel]]
 *
 * [[scalats.TsParser.parseTopLevel]] takes a type `A` and replaces its type parameters
 * with references to [[scalats.TypeParam]], while [[scalats.TsParser.parse]] treats types as is.
 */
final class TsParser()(using override val ctx: Quotes) extends ReflectionUtils {
  import ctx.reflect.*

  /** A predefined set of [[scalats.TypeParam]] instances to use as references in parameterized types */
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

  /**
   * Parse a given type `A` into a [[scalats.TsModel]], replacing the type parameters of `A` (if any)
   * with references to [[scalats.TypeParam]].
   *
   * @tparam A The type to parse
   * @return The [[scalats.TsModel]] representation of `A`
   */
  def parseTopLevel[A: Type]: Expr[TsModel] =
    TypeRepr.of[A] match {
      case AppliedType(tpe, params) if params.nonEmpty =>
        AppliedType(tpe, params.zipWithIndex.map { case (_, i) => typeParamTypes(i) }).asType match {
          case '[t] => parse[t](true)
        }
      case _ =>
        parse[A](true)
    }

  /**
   * Parse a given type `A` into a [[scalats.TsModel]]. When `top` is `true`, `enum`s, `case class`es, and `object`s
   * will be parsed as definitions. When `top` is `false`, they will be parsed as references to the type.
   *
   * @tparam A The type to parse
   * @param top Whether the type should be parsed as a top-level definition
   * @return The [[scalats.TsModel]] representation of `A`
   */
  def parse[A: Type](top: Boolean): Expr[TsModel] = {
    val typeRepr = TypeRepr.of[A]
    val typeName = mkTypeName(typeRepr)

    typeRepr.asType match {
      case '[Nothing] => '{ TsModel.Unknown($typeName, Nil) }
      case '[TypeParam[a]] =>
        @annotation.nowarn("msg=match may not be exhaustive")
        val name = TypeRepr.of[a] match { case ConstantType(c) => c.value match { case s: String => s } }
        '{ TsModel.TypeParam(${ Expr(name) }) }
      case '[io.circe.Json] => '{ TsModel.Json($typeName) }
      case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => '{ TsModel.Number($typeName) }
      case '[BigDecimal] | '[BigInt] => '{ TsModel.BigNumber($typeName) }
      case '[Boolean] => '{ TsModel.Boolean($typeName) }
      case '[String] => '{ TsModel.String($typeName) }
      case '[java.time.LocalDate] | '[org.joda.time.LocalDate] => '{ TsModel.LocalDate($typeName) }
      case '[java.time.LocalDateTime] | '[java.time.ZonedDateTime] | '[java.time.Instant] | '[org.joda.time.DateTime] => '{ TsModel.DateTime($typeName) }
      case '[java.util.UUID] => '{ TsModel.UUID($typeName) }
      case '[cats.Eval[a]] => '{ TsModel.Eval($typeName, ${ parse[a](false) }) }
      case '[cats.data.Chain[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.Chain[Any]].toList) }
      case '[List[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[List[Any]]) }
      case '[Vector[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[Vector[Any]].toList) }
      case '[Seq[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[Seq[Any]].toList) }
      case '[Set[a]] => '{ TsModel.Set($typeName, ${ parse[a](false) }) }
      case '[collection.immutable.SortedSet[a]] => '{ TsModel.Set($typeName, ${ parse[a](false) }) }
      case '[cats.data.NonEmptyChain[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyChain[Any]].toNonEmptyList) }
      case '[cats.data.NonEmptyList[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyList[Any]]) }
      case '[cats.data.NonEmptyVector[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyChain[Any]].toNonEmptyList) }
      case '[scalaz.NonEmptyList[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[scalaz.NonEmptyList[Any]].pipe(l => cats.data.NonEmptyList(l.head, l.tail.toList))) }
      case '[Option[a]] => '{ TsModel.Option($typeName, ${ parse[a](false) }) }
      case '[Either[l, r]] => '{ TsModel.Either($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[Either[Any, Any]]) }
      case '[scalaz.\/[l, r]] => '{ TsModel.Either($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[scalaz.\/[Any, Any]].toEither) }
      case '[cats.data.Ior[l, r]] => '{ TsModel.Ior($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[cats.data.Ior[Any, Any]]) }
      case '[scalaz.\&/[l, r]] => '{
        TsModel.Ior($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[scalaz.\&/[Any, Any]]
          .fold(cats.data.Ior.Left(_), cats.data.Ior.Right(_), cats.data.Ior.Both(_, _)))
      }
      case '[Map[k, v]] => '{ TsModel.Map($typeName, ${ parse[k](false) }, ${ parse[v](false) }) }
      case '[EmptyTuple] => '{ TsModel.Tuple($typeName, Nil) }
      case '[*:[h, t]] =>
        def unroll[T <: Tuple: Type]: List[Expr[TsModel]] =
          Type.of[T] match {
            case '[*:[h, t]] => parse[h](false) :: unroll[t]
            case '[EmptyTuple] => Nil
          }
        '{ TsModel.Tuple($typeName, ${ Expr.ofList(unroll[h *: t]) }) }
      case '[t] if typeRepr <:< TypeRepr.of[Tuple] =>
        '{ TsModel.Tuple($typeName, ${ mkTypeArgs(TypeRepr.of[t]) }) }
      case '[t] =>
        Mirror(typeRepr) match {
          case Some(m) =>
            m.mirrorType match {
              case MirrorType.Sum => if (top) parseEnum[t](m) else parseEnumRef[t](m)
              case MirrorType.Product => if (top) parseCaseClass[t](m, None) else parseCaseClassRef[t]
              case MirrorType.Singleton => if (top) parseObject[t](None) else parseObjectRef[t]
            }

          case None =>
            '{ TsModel.Unknown($typeName, ${ mkTypeArgs(typeRepr) }) }
        }
    }
  }

  private def mkTypeName(typeRepr: TypeRepr): Expr[TypeName] =
    '{ T(${ Expr(typeRepr.show) }) }

  private def mkTypeArgs(typeRepr: TypeRepr): Expr[List[TsModel]] =
    Expr.ofList(typeRepr.typeArgs.map(_.asType match { case '[a] => parse[a](false) }))

  /** Parse an `enum` definiton into its [[scalats.TsModel]] representation */
  private def parseEnum[A: Type](mirror: Mirror): Expr[TsModel] = {
    def getTypeConstructor(t: TypeRepr): TypeRepr = t match {
      case AppliedType(t, _) => t
      case _ => t
    }

    val typeRepr = TypeRepr.of[A]
    val tycon = getTypeConstructor(typeRepr)

    def parseMembers(m: Mirror): List[Expr[TsModel.Object | TsModel.Interface]] = {
      m.mirrorType match {
        case MirrorType.Sum =>
          m.types.toList
            .filter(t => getTypeConstructor(t.baseType(tycon.typeSymbol)) =:= tycon)
            .flatMap(Mirror(_).fold(Nil)(parseMembers))
        case MirrorType.Product => List(m.mirroredType.asType match { case '[t] => parseCaseClass[t](m, Some(typeRepr)) })
        case MirrorType.Singleton => List(m.mirroredType.asType match { case '[t] => parseObject[t](Some(typeRepr)) })
      }
    }

    '{
      TsModel.Union(
        ${ mkTypeName(typeRepr) },
        ${ mkTypeArgs(typeRepr) },
        ${ Expr.ofList(parseMembers(mirror)) }.distinctBy(_.typeName.raw)
      )
    }
  }

  private def parseEnumRef[A: Type](mirror: Mirror): Expr[TsModel.UnionRef] = {
    val typeRepr = TypeRepr.of[A]

    def allObjects(m: Mirror): Boolean =
      m.mirrorType match {
        case MirrorType.Sum => m.types.toList.foldLeft(true)((acc, t) => Mirror(t).fold(acc)(m => acc && allObjects(m)))
        case MirrorType.Product => false
        case MirrorType.Singleton => true
      }

    '{
      TsModel.UnionRef(
        ${ mkTypeName(typeRepr) },
        ${ mkTypeArgs(typeRepr) },
        ${ Expr(allObjects(mirror)) },
      )
    }
  }

  /** Parse a `case class` definiton into its [[scalats.TsModel]] representation */
  private def parseCaseClass[A: Type](mirror: Mirror, parent: Option[TypeRepr]): Expr[TsModel.Interface] = {
    val typeRepr = TypeRepr.of[A]
    val fields = mirror.types.toList.zip(mirror.labels).map { case (tpe, name) =>
      tpe.asType match { case '[t] => '{ TsModel.InterfaceField(${ Expr(name) }, ${ parse[t](false) }) } }
    }
    '{
      TsModel.Interface(
        ${ mkTypeName(typeRepr) },
        ${ Expr(parent.map(_.show)) }.map(T(_)),
        ${ mkTypeArgs(typeRepr) },
        ${ Expr.ofList(fields) },
      )
    }
  }

  private def parseCaseClassRef[A: Type]: Expr[TsModel.InterfaceRef] = {
    val typeRepr = TypeRepr.of[A]
    '{ TsModel.InterfaceRef(${ mkTypeName(typeRepr) }, ${ mkTypeArgs(typeRepr) }) }
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
          s.flags.is(Flags.Given) ||
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

    unAnd(typeRepr.memberType(sym))
  }

  /** Parse an `object` definiton into its [[scalats.TsModel]] representation */
  private def parseObject[A: Type](parent: Option[TypeRepr]): Expr[TsModel.Object] = {
    val typeRepr = TypeRepr.of[A]

    val value = Expr.summon[ValueOf[A]].getOrElse(report.errorAndAbort(s"Unable to summon `ValueOf[${typeRepr.show}]`"))
    val members = valMembers(typeRepr)
    val fields = Expr.ofList(members.map(s => valDefType(typeRepr, s).asType match {
      case '[a] => '{ TsModel.ObjectField(${ Expr(s.name) }, ${ parse[a](false) }, ${ Select('{ $value.value }.asTerm, s).asExpr }) }
    }))
    '{
      TsModel.Object(
        ${ mkTypeName(typeRepr) },
        ${ Expr(parent.map(_.show)) }.map(T(_)),
        $fields,
      )
    }
  }

  private def parseObjectRef[A: Type]: Expr[TsModel.ObjectRef] =
    '{ TsModel.ObjectRef(${ mkTypeName(TypeRepr.of[A]) }) }
}
