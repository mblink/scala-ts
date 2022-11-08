package com.mpc.scalats
package core

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe._

final class ScalaParser(logger: Logger, mirror: Mirror, excludeType: Type => Boolean)(implicit config: configuration.Config) {
  import ScalaModel._

  def parseTypes(types: List[Type]): ListSet[TypeDef] =
    parse(types, ListSet[Type](), ListSet.empty[TypeDef])

  def isNotExcluded(tpe: Type): Boolean =
    !excludeType(tpe)

  private def getTypeParams(tpe: Type): Set[String] =
    tpe.typeParams.map(_.name.decodedName.toString).toSet

  private object TaggedTypeExtractor {
    def unapply(tpe: Type): Option[(Type, Type)] =
      (config.scalaTagTypeName, tpe.typeSymbol, tpe.typeArgs, tpe.dealias.typeArgs) match {
        case (Some(tagTpeName), sym, Nil, List(baseTpe, tagTpe)) if sym.isType &&
                                                                    sym.asType.toType.typeConstructor.toString == tagTpeName =>
          Some((baseTpe, tagTpe))

        case _ => None
      }
  }

  private def parseType(tpe: Type): (ListSet[Type], Option[TypeDef]) = {
    if (isNotExcluded(tpe)) {
      tpe match {
        case _: SingleTypeApi =>
          (ListSet(tpe), parseObject(tpe))

        case _ if (tpe.getClass.getName contains "ModuleType" /*Workaround*/) =>
          (ListSet(tpe), parseObject(tpe))

        case TaggedTypeExtractor(baseTpe, tagTpe) =>
          (ListSet(tpe), parseTaggedType(tpe, baseTpe, tagTpe))

        case _ if tpe.typeSymbol.isClass && !tpe.typeSymbol.name.toString.contains("NonEmptyList") =>
          if (isSealedUnion(tpe)) {
            parseSealedUnion(tpe)
          } else if (isCaseClass(tpe) && !isAnyValChild(tpe)) {
            (ListSet(tpe), parseCaseClass(tpe))
          } else if (tpe.dealias != tpe) { // if the dealiased type is different then it's a type alias
            (ListSet(tpe), parseTypeAlias(tpe))
          } else {
            (ListSet(), None)
          }

        case _ =>
          logger.warning(s"Unsupported Scala type: $tpe")
          (ListSet(), None)
      }
    } else {
      (ListSet(), None)
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
        val symTpe = getTypeInfo(t)
        val tp = (if (t.isClass) symTpe.typeSymbol.asClass.typeParams else List[Symbol]()).map(_.name.toString)
        TypeMember(t.name.toString, getTypeRef(symTpe, tp.toSet), Some(v))
      }
      case m: MethodSymbol if isValidMethod(m) => {
        val sm = mirror.staticModule(tpe.typeSymbol.fullName.stripSuffix(".type"))
        val v = mirror.reflect(mirror.reflectModule(sm).instance).reflectMethod(m)

        if (m.paramLists.isEmpty) {
          TypeMember(m.name.toString, getTypeRef(trueType(getTypeInfo(m), Some(tpe)), Set()), Some(v()))
        } else {
          TypeMember(m.name.toString, getTypeRef(getTypeInfo(m), Set.empty), None)
        }
      }
    }

    val members = tpe.decls.sorted.collect {
      case Field(m) if !m.isStatic => member(m, Set())
    }

    Some(CaseObject(
      tpe.typeSymbol.name.toString stripSuffix ".type",
      tpe.toString,
      ListSet.empty ++ statics ++ members,
      tpe))
  }

  private def parseSealedUnion(tpe: Type): (ListSet[Type], Option[SealedUnion]) = {
    // Members
    val members = tpe.members.sorted.collect {
      case m: MethodSymbol if isValidMethod(m) && !m.name.toString.endsWith("$") =>
        member(m, Set())
    }

    val (subclassExamined, subclassTypeDefs) = directKnownSubclasses(tpe).foldLeft((ListSet.empty[Type], ListSet.empty[TypeDef])) {
      case ((accExamined, accTypeDefs), subclass) =>
        val (examined, typeDef) = parseType(subclass)
        (accExamined ++ examined, accTypeDefs ++ typeDef)
    }

    if (subclassTypeDefs.nonEmpty) {
      (subclassExamined + tpe, Some(SealedUnion(
        tpe.typeSymbol.name.toString,
        tpe.toString,
        ListSet.empty ++ members,
        ListSet.empty ++ getTypeParams(tpe),
        subclassTypeDefs,
        tpe)))
    } else {
      (ListSet(tpe), Option.empty[SealedUnion])
    }
  }

  private def parseCaseClass(caseClassType: Type): Option[CaseClass] = {
    val typeParams = getTypeParams(caseClassType)

    // Members
    val members = caseClassType.members.sorted.collect {
      case Field(m) if m.isCaseAccessor => member(m, typeParams)
    }.toList

    val values = caseClassType.decls.sorted.collect {
      case Field(m) => member(m, typeParams)
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

  private def parseTaggedType(tpe: Type, baseTpe: Type, tagTpe: Type): Option[TaggedType] =
    Some(TaggedType(
      tpe.toString.split('.').last,
      tpe.toString,
      getTypeRef(baseTpe, Set()),
      tagTpe.toString.split('.').last,
      tpe))

  @inline private def trueType(tpe: Type, seenFrom: Option[Type]): Type =
    seenFrom.fold(tpe)(t => tpe.asSeenFrom(t, t.typeSymbol)).map(_.dealias)

  /* The last overridden symbol is the furthest up the inheritance hierarchy, so it has the true type of the symbol, e.g.

    sealed trait Foo { val x: Option[Int] }
    sealed trait Bar { val x = Some(1) }
    sealed trait Baz { override val x = Some(2) }

    val bazX = typeOf[Baz].members.find(_.name.toString == "x").get

    // Baz#x incorrectly has type `Some[Int]`
    bazX.info // => Some[Int]
    // Baz#x has two overridden symbols
    bazX.overrides // => List(value x, value x)
    // The last override of Baz#x refers to Foo#x with the correct type `Option[Int]`
    bazX.overrides.map(_.info) // => List(Some[Int], Option[Int])
  */
  @annotation.tailrec
  private def getTypeInfo(sym: Symbol): Type =
    (sym, sym.overrides.lastOption) match {
      case (_, Some(o)) => getTypeInfo(o)
      case (m: MethodSymbol, None) => m.returnType
      case (s, None) => s.info
    }

  @inline private def member(sym: MethodSymbol, typeParams: Set[String]): TypeMember =
    TypeMember(sym.name.toString, getTypeRef(getTypeInfo(sym), typeParams))

  @tailrec
  private def parse(types: List[Type], examined: ListSet[Type], parsed: ListSet[TypeDef]): ListSet[TypeDef] =
    types.filter(isNotExcluded) match {
      case scalaType :: tail =>
        if (examined.contains(scalaType) || scalaType.typeSymbol.isParameter) {
          parse(tail, examined, parsed)
        } else {
          val (parsedExamined, parsedTypeDef) = parseType(scalaType)
          parse(tail, examined ++ parsedExamined, parsed ++ parsedTypeDef)
        }

      case _ => parsed
    }

  private val tupleName = """^Tuple(\d+)$""".r
  private val nothingTypeRef = UnknownTypeRef("Nothing", typeOf[Nothing])

  // TODO: resolve from implicit (typeclass)
  private def getTypeRef(scalaType: Type, typeParams: Set[String]): TypeRef = {
    (scalaType, scalaType.typeSymbol.fullName, scalaType.typeSymbol.name.toString, scalaType.dealias.typeArgs) match {
      case (_, _, "Int" | "Byte" | "Short", _) =>
        IntRef
      case (_, _, "Long", _) =>
        LongRef
      case (_, _, "Double", _) =>
        DoubleRef
      case (_, _, "Boolean", _) =>
        BooleanRef
      case (_, _, "String", _) =>
        StringRef
      case (_, _, "Nil", _) =>
        SeqRef(nothingTypeRef)
      case (_, _, "List" | "Seq" | "Vector", List(innerType)) => // TODO: Iterable
        SeqRef(getTypeRef(innerType, typeParams))
      case (_, "cats.Eval", _, List(innerType)) =>
        getTypeRef(innerType, getTypeParams(innerType))
      case (_, "cats.data.Chain", _, List(innerType)) =>
        SeqRef(getTypeRef(innerType, typeParams))
      case (_, _, "Set" | "SortedSet", List(innerType)) =>
        SetRef(getTypeRef(innerType, typeParams))
      case (_, _, "NonEmptyChain" | "NonEmptyList" | "NonEmptyVector", List(innerType)) =>
        NonEmptySeqRef(getTypeRef(innerType, typeParams))
      case (_, _, "Option" | "Some", List(innerType)) =>
        OptionRef(getTypeRef(innerType, typeParams))
      case (_, _, "None", Nil) =>
        OptionRef(nothingTypeRef)
      case (_, _, "LocalDate", _) =>
        DateRef
      case (_, _, "Instant" | "Timestamp" | "LocalDateTime" | "ZonedDateTime" | "DateTime", _) =>
        DateTimeRef
      case (_, _, tupleName(_), typeArgs) =>
        TupleRef(ListSet.empty ++ typeArgs.map(getTypeRef(_, Set())))
      case (_, _, typeParam, _) if typeParams.contains(typeParam) =>
        TypeParamRef(typeParam, scalaType)
      case _ if isAnyValChild(scalaType) =>
        getTypeRef(scalaType.members.filter(!_.isMethod).map(_.typeSignature).head, Set())
      case (_, _, _, typeArgs) if isSealedUnion(scalaType) =>
        UnionRef(
          scalaType.typeSymbol.name.toString,
          ListSet.empty ++ typeArgs.map(getTypeRef(_, Set())),
          ListSet.empty ++ directKnownSubclasses(scalaType).map(getTypeRef(_, Set.empty)),
          scalaType)
      case (_, _, _, typeArgs) if isCaseClass(scalaType) =>
        val caseClassName = scalaType.typeSymbol.name.toString
        val typeArgRefs = typeArgs.map(getTypeRef(_, typeParams))

        CaseClassRef(caseClassName, scalaType.toString, ListSet.empty ++ typeArgRefs, scalaType)

      case (_, _, "Either" | """\/""", List(lType, rType)) =>
        EitherRef(getTypeRef(lType, typeParams), getTypeRef(rType, typeParams))

      case (_, _, "Ior" | """\&/""", List(lType, rType)) =>
        TheseRef(getTypeRef(lType, typeParams), getTypeRef(rType, typeParams))

      case (_, _, "Map", List(kType, vType)) =>
        MapRef(getTypeRef(kType, typeParams), getTypeRef(vType, typeParams))

      case (TaggedTypeExtractor(_, _), _, _, _) =>
        TaggedTypeRef(scalaType.toString.split('.').last, scalaType)

      case (_, _, unknown, _) =>
        UnknownTypeRef(unknown, scalaType)
    }
  }

  @inline private def isSealedUnion(scalaType: Type): Boolean =
    scalaType.typeSymbol.isClass && scalaType.typeSymbol.asClass.isTrait && scalaType.typeSymbol.asClass.isSealed

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
        val newSub: ListSet[Type] = if (cls.isCaseClass) ListSet(cls.selfType.typeConstructor) else {
          logger.warning(s"cannot handle class ${cls.fullName}: no case accessor")
          ListSet.empty
        }

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
