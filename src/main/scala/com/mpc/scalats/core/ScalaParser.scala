package com.mpc.scalats.core

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe._

final class ScalaParser(logger: Logger, mirror: Mirror, excludeType: Type => Boolean) {
  import ScalaModel._

  def parseTypes(types: List[Type]): ListSet[TypeDef] =
    parse(types, ListSet[Type](), ListSet.empty[TypeDef])

  def isNotExcluded(tpe: Type): Boolean =
    !excludeType(tpe)

  private def getTypeParams(tpe: Type): Set[String] =
    tpe.typeParams.map(_.name.decodedName.toString).toSet

  private def parseType(tpe: Type): Option[TypeDef] = {
    if (isNotExcluded(tpe)) {
      tpe match {
        case _: SingleTypeApi =>
          parseObject(tpe)

        case _ if (tpe.getClass.getName contains "ModuleType" /*Workaround*/) =>
          parseObject(tpe)

        case _ if tpe.typeSymbol.isClass && !tpe.typeSymbol.name.toString.contains("NonEmptyList") =>
          if (isSealedUnion(tpe)) {
            parseSealedUnion(tpe)
          } else if (isCaseClass(tpe) && !isAnyValChild(tpe)) {
            parseCaseClass(tpe)
          } else if (tpe.dealias != tpe) { // if the dealiased type is different then it's a type alias
            parseTypeAlias(tpe)
          } else {
            None
          }

        case _ =>
          logger.warning(s"Unsupported Scala type: $tpe")
          None
      }
    } else {
      None
    }
  }

  private object Field {
    def unapply(m: MethodSymbol): Option[MethodSymbol] = m match {
      case m: MethodSymbol if (isValidMethod(m) &&
          m.paramLists.forall(_.isEmpty) &&
          {
            val n = m.name.toString
            !(n.contains("$") || n.startsWith("<"))
          } &&
          m.overrides.forall { o =>
            val declaring = o.owner.fullName

            !declaring.startsWith("java.") &&
            !declaring.startsWith("scala.")
          }) => Some(m)

      case _ => None
    }
  }

  private def parseObject(tpe: Type): Option[CaseObject] = {
    val statics = tpe.members.sorted.collect {
      case t: TermSymbol if t.isVal && t.isStable && t.isStatic => {
        val sm = mirror.staticModule(tpe.typeSymbol.fullName.stripSuffix(".type"))
        val v = mirror.reflect(mirror.reflectModule(sm).instance).reflectField(t).get
        val tp = (if (t.isClass) t.info.typeSymbol.asClass.typeParams else List[Symbol]()).map(_.name.toString)
        TypeMember(t.name.toString, getTypeRef(t.info, tp.toSet), Some(v))
      }
      case m: MethodSymbol if isValidMethod(m) => {
        val sm = mirror.staticModule(tpe.typeSymbol.fullName.stripSuffix(".type"))
        val v = mirror.reflect(mirror.reflectModule(sm).instance).reflectMethod(m)

        if (m.paramLists.isEmpty) {
          TypeMember(m.name.toString, getTypeRef(trueType(m.returnType, Some(tpe)), Set()), Some(v()))
        } else {
          TypeMember(m.name.toString, getTypeRef(m.returnType, Set.empty), None)
        }
      }
    }

    val members = tpe.decls.sorted.collect {
      case Field(m) if !m.isStatic => {
        member(tpe, m, Set())
      }
    }

    Some(CaseObject(
      tpe.typeSymbol.name.toString stripSuffix ".type",
      tpe.toString,
      ListSet.empty ++ statics ++ members,
      tpe))
  }

  private def parseSealedUnion(tpe: Type): Option[SealedUnion] = {
    // TODO: Check & warn there is no type parameters for a union type

    // Members
    val members = tpe.members.sorted.collect {
      case m: MethodSymbol if isValidMethod(m) && !m.name.toString.endsWith("$") =>
        member(tpe, m, Set())
    }

    directKnownSubclasses(tpe) match {
      case possibilities @ (_ :: _ ) =>
        Some(SealedUnion(
          tpe.typeSymbol.name.toString,
          tpe.toString,
          ListSet.empty ++ members,
          parseTypes(possibilities),
          tpe))

      case _ => Option.empty[SealedUnion]
    }
  }

  private def parseCaseClass(caseClassType: Type): Option[CaseClass] = {
    val typeParams = getTypeParams(caseClassType)

    // Members
    val members = caseClassType.members.sorted.collect {
      case Field(m) if m.isCaseAccessor =>
        member(caseClassType, m, typeParams)
    }.toList

    val values = caseClassType.decls.sorted.collect {
      case Field(m) =>
        member(caseClassType, m, typeParams)
    }.filterNot(members.contains)

    Some(CaseClass(
      caseClassType.typeSymbol.name.toString,
      caseClassType.toString,
      ListSet.empty ++ members,
      ListSet.empty ++ values,
      ListSet.empty ++ typeParams,
      caseClassType
    ))
  }

  private def parseTypeAlias(tpe: Type): Option[TypeAlias] = {
    val typeParams = getTypeParams(tpe)

    Some(TypeAlias(
      tpe.toString.split('.').last,
      tpe.toString,
      getTypeRef(tpe.dealias, typeParams),
      ListSet.empty ++ typeParams,
      tpe))
  }

  @inline private def trueType(tpe: Type, seenFrom: Option[Type]): Type =
    seenFrom.fold(tpe)(t => tpe.asSeenFrom(t, t.typeSymbol)).map(_.dealias)

  @inline private def member(owner: Type, sym: MethodSymbol, typeParams: Set[String]): TypeMember =
    TypeMember(sym.name.toString, getTypeRef(trueType(sym.returnType, Some(owner)), typeParams))

  @annotation.tailrec
  private def parse(types: List[Type], examined: ListSet[Type], parsed: ListSet[TypeDef]): ListSet[TypeDef] =
    types.filter(isNotExcluded) match {
      case scalaType :: tail => {
        if (!examined.contains(scalaType) && !scalaType.typeSymbol.isParameter) {

          val relevantMemberSymbols = scalaType.members.collect {
            case m: MethodSymbol if isValidMethod(m) => m
          }

          val memberTypes = relevantMemberSymbols.map(
            _.typeSignature.map(_.dealias) match {
              case NullaryMethodType(resultType) => resultType
              case t => t.map(_.dealias)
            })

          val typeArgs = scalaType match {
            case t: scala.reflect.runtime.universe.TypeRef => t.args
            case _ => List.empty[Type]
          }

          parse(
            memberTypes ++: typeArgs ++: tail,
            examined + scalaType,
            parsed ++ parseType(scalaType))

        } else {
          parse(tail, examined + scalaType, parsed ++ parseType(scalaType))
        }
      }

      case _ => parsed
    }

  private val tupleName = """^Tuple(\d+)$""".r

  // TODO: resolve from implicit (typeclass)
  private def getTypeRef(scalaType: Type, typeParams: Set[String]): TypeRef = {
    scalaType.typeSymbol.name.toString match {
      case "Int" | "Byte" | "Short" =>
        IntRef
      case "Long" =>
        LongRef
      case "Double" =>
        DoubleRef
      case "Boolean" =>
        BooleanRef
      case "String" =>
        StringRef
      case "List" | "Seq" | "Set" | "Vector" => // TODO: Iterable
        val innerType = scalaType.typeArgs.head
        SeqRef(getTypeRef(innerType, typeParams))
      case "NonEmptyList" =>
        val innerType = scalaType.typeArgs.head
        NonEmptySeqRef(getTypeRef(innerType, typeParams))
      case "Option" =>
        val innerType = scalaType.typeArgs.head
        OptionRef(getTypeRef(innerType, typeParams))
      case "LocalDate" =>
        DateRef
      case "Instant" | "Timestamp" | "LocalDateTime" | "ZonedDateTime" | "DateTime" =>
        DateTimeRef
      case tupleName(_) =>
        TupleRef(ListSet.empty ++ scalaType.typeArgs.map(getTypeRef(_, Set())))
      case typeParam if typeParams.contains(typeParam) =>
        TypeParamRef(typeParam, scalaType)
      case _ if isAnyValChild(scalaType) =>
        getTypeRef(scalaType.members.filter(!_.isMethod).map(_.typeSignature).head, Set())
      case _ if isSealedUnion(scalaType) =>
        UnionRef(
          scalaType.typeSymbol.name.toString,
          ListSet.empty ++ directKnownSubclasses(scalaType).map(getTypeRef(_, Set.empty)),
          scalaType)
      case _ if isCaseClass(scalaType)=>
        val caseClassName = scalaType.typeSymbol.name.toString
        val typeArgs = scalaType.typeArgs
        val typeArgRefs = typeArgs.map(getTypeRef(_, typeParams))

        CaseClassRef(caseClassName, scalaType.toString, ListSet.empty ++ typeArgRefs, scalaType)

      case "Either" | """\/""" | "Disjunction" =>
        EitherRef(getTypeRef(scalaType.typeArgs.head, typeParams), getTypeRef(scalaType.typeArgs.last, typeParams))

      case "Ior" | """\&/""" =>
        TheseRef(getTypeRef(scalaType.typeArgs.head, typeParams), getTypeRef(scalaType.typeArgs.last, typeParams))

      case "Map" =>
        val keyType = scalaType.typeArgs.head
        val valueType = scalaType.typeArgs.last
        MapRef(getTypeRef(keyType, typeParams), getTypeRef(valueType, typeParams))

      case unknown =>
        UnknownTypeRef(unknown, scalaType)
    }
  }

  @inline private def isSealedUnion(scalaType: Type): Boolean =
    scalaType.typeSymbol.isClass && scalaType.typeSymbol.asClass.isTrait &&
      scalaType.typeSymbol.asClass.isSealed && scalaType.typeParams.isEmpty

  @inline private def isCaseClass(scalaType: Type): Boolean =
    scalaType.typeSymbol.isClass && scalaType.typeSymbol.asClass.isCaseClass &&
      tupleName.unapplySeq(scalaType.typeSymbol.name.toString).isEmpty

  @inline private def isAnyValChild(scalaType: Type): Boolean =
    scalaType <:< typeOf[AnyVal]

  @inline private def isValidMethod(m: MethodSymbol): Boolean =
    m.isPublic && m.isMethod && m.isAccessor && m.isTerm &&
    !m.isConstructor && !m.isStatic && m.returnType != typeOf[Unit] &&
    !m.isImplicit

  private def decls(sym: Symbol): List[Symbol] = sym.typeSignature.decls.sorted

  private def sealedAbstractClass(sym: Symbol): Option[ClassSymbol] =
    if (sym.isClass) Some(sym.asClass).filter(s => s.isSealed && s.isAbstract)
    else None

  private def directKnownSubclasses(tpe: Type): List[Type] = {
    // Workaround for SI-7046: https://issues.scala-lang.org/browse/SI-7046
    lazy val tpeSym = tpe.typeSymbol.asClass
    lazy val tpeSymParents = tpeSym.baseClasses.flatMap(sealedAbstractClass)

    @annotation.tailrec
    def allSubclasses(path: Iterable[Symbol], subclasses: ListSet[Type]): ListSet[Type] = path.headOption match {
      case Some(cls: ClassSymbol) if (
        tpeSym != cls && cls.selfType.baseClasses.contains(tpeSym)) => {
        val newSub: ListSet[Type] = if (!cls.isCaseClass) {
          logger.warning(s"cannot handle class ${cls.fullName}: no case accessor")
          ListSet.empty
        } else if (cls.typeParams.nonEmpty) {
          logger.warning(s"cannot handle class ${cls.fullName}: type parameter not supported")
          ListSet.empty
        } else ListSet(cls.selfType)

        allSubclasses(path.tail, subclasses ++ newSub)
      }

      case Some(o: ModuleSymbol) if (
        o != NoSymbol &&
          o == tpeSym.companion && o.companion == tpeSym // companion object of the given type
      ) =>
        allSubclasses(path.tail ++ decls(o), subclasses)

      case Some(o: ModuleSymbol) if (
        o != NoSymbol &&
          tpeSymParents.exists(p => o == p.companion && o.companion == p) // companion of a parent of the given type
      ) =>
        allSubclasses(path.tail ++ decls(o), subclasses)

      case Some(o: ModuleSymbol) if (
        o.companion == NoSymbol && // not a companion object
          o.typeSignature.baseClasses.contains(tpeSym)) =>
        allSubclasses(path.tail, subclasses + o.typeSignature)

      case Some(o: ModuleSymbol) if (
        o.companion == NoSymbol // not a companion object
      ) => allSubclasses(path.tail, subclasses)

      case Some(_) => allSubclasses(path.tail, subclasses)

      case _ => subclasses
    }

    sealedAbstractClass(tpeSym)
      .map(s => allSubclasses(decls(s.owner), ListSet.empty).toList)
      .getOrElse(Nil)
  }
}

object ScalaParser {
  def parseTypeParams(typeParams: String): List[String] = {
    @tailrec
    def go(prev: List[String], curr: StringBuilder, chars: List[Char], openBrackets: List[Char]): List[String] =
      (chars, openBrackets) match {
        case (',' :: rest, Nil) =>
          go(prev :+ curr.toString, new StringBuilder(""), rest, Nil)

        case ((c @ ',') :: rest, bs) =>
          go(prev, curr += c, rest, bs)

        case (']' :: _, Nil) =>
          sys.error(s"Unexpected closing bracket `]` in string $typeParams")

        case ((c @ ']') :: rest, _ :: bs) =>
          go(prev, curr += c, rest, bs)

        case ((c @ '[') :: rest, bs) =>
          go(prev, curr += c, rest, c :: bs)

        case (c :: rest, bs) =>
          go(prev, curr += c, rest, bs)

        case (Nil, _) =>
          prev :+ curr.toString
      }

    Option(typeParams).filter(_.nonEmpty).fold(List[String]())(
      tps => go(Nil, new StringBuilder(""), tps.toList, Nil))
  }
}
