package com.mpc.scalats.core

/**
  * Created by Milosz on 09.06.2016.
  */

import scala.collection.immutable.ListSet

import scala.reflect.runtime.universe._

// TODO: Keep namespace using fullName from the Type
final class ScalaParser(logger: Logger, mirror: Mirror, excludeTypes: List[Type]) {

  import ScalaModel._

  def parseTypes(types: List[Type]): ListSet[TypeDef] =
    parse(types, ListSet[Type](), ListSet.empty[TypeDef])


  private def parseType(tpe: Type): Option[TypeDef] =
    if (!excludeTypes.contains(tpe)) {
      tpe match {
        case _: SingleTypeApi =>
          parseObject(tpe)

        case _ if (tpe.getClass.getName contains "ModuleType" /*Workaround*/) =>
          parseObject(tpe)

        case _ if tpe.typeSymbol.isClass && !tpe.typeSymbol.name.toString.contains("NonEmptyList") => {
          if (isSealedUnion(tpe)) {
            parseSealedUnion(tpe)
          } else if (isCaseClass(tpe) && !isAnyValChild(tpe)) {
            parseCaseClass(tpe)
          } else {
            None
          }
        }

        case _ => {
          logger.warning(s"Unsupported Scala type: $tpe")
          None
        }
      }
    } else {
      None
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
          TypeMember(m.name.toString, getTypeRef(m.returnType, Set.empty), Some(v()))
        } else {
          TypeMember(m.name.toString, getTypeRef(m.returnType, Set.empty), None)
        }
      }
    }

    val members = tpe.decls.sorted.collect {
      case Field(m) if !m.isStatic => {
        member(m, List.empty)
      }
    }

    Some(CaseObject(
      tpe.typeSymbol.name.toString stripSuffix ".type",
      ListSet.empty ++ statics ++ members))
  }

  private def parseSealedUnion(tpe: Type): Option[SealedUnion] = {
    // TODO: Check & warn there is no type parameters for a union type

    // Members
    val members = tpe.members.sorted.collect {
      case m: MethodSymbol if isValidMethod(m) && !m.name.toString.endsWith("$") =>
        member(m, List.empty)
    }

    directKnownSubclasses(tpe) match {
      case possibilities @ (_ :: _ ) =>
        Some(SealedUnion(
          tpe.typeSymbol.name.toString,
          ListSet.empty ++ members,
          parseTypes(possibilities.reverse)))

      case _ => Option.empty[SealedUnion]
    }
  }

  private def parseCaseClass(caseClassType: Type): Option[CaseClass] = {
    val typeParams = caseClassType.typeConstructor.
      dealias.typeParams.map(_.name.decodedName.toString)

    // Members
    val members = caseClassType.members.sorted.collect {
      case Field(m) if m.isCaseAccessor =>
        member(m, typeParams)
    }.toList

    val values = caseClassType.decls.sorted.collect {
      case Field(m) =>
        member(m, typeParams)
    }.filterNot(members.contains)

    Some(CaseClass(
      caseClassType.typeSymbol.name.toString,
      ListSet.empty ++ members,
      ListSet.empty ++ values,
      ListSet.empty ++ typeParams
    ))
  }

  @inline private def member(
    sym: MethodSymbol, typeParams: List[String]
  ) = TypeMember(sym.name.toString, getTypeRef(
    sym.returnType.map(_.dealias), typeParams.toSet))

  @annotation.tailrec
  private def parse(types: List[Type], examined: ListSet[Type], parsed: ListSet[TypeDef]): ListSet[TypeDef] = types match {
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
      case "List" | "Seq" | "Set" => // TODO: Traversable
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
      case typeParam if typeParams.contains(typeParam) =>
        TypeParamRef(typeParam)
      case _ if isAnyValChild(scalaType) =>
        getTypeRef(scalaType.members.filter(!_.isMethod).map(_.typeSignature).head, Set())
      case _ if isSealedUnion(scalaType) =>
          UnionRef(ListSet.empty ++ directKnownSubclasses(scalaType).map(getTypeRef(_, Set.empty)))
      case _ if isCaseClass(scalaType)=>
        val caseClassName = scalaType.typeSymbol.name.toString
        val typeArgs = scalaType.typeArgs
        val typeArgRefs = typeArgs.map(getTypeRef(_, typeParams))

        CaseClassRef(caseClassName, ListSet.empty ++ typeArgRefs)

      case "Either" => {
        val innerTypeL = scalaType.typeArgs.head
        val innerTypeR = scalaType.typeArgs.last

        UnionRef(ListSet(
          getTypeRef(innerTypeL, typeParams),
          getTypeRef(innerTypeR, typeParams)))
      }

      case "Map" =>
        val keyType = scalaType.typeArgs.head
        val valueType = scalaType.typeArgs.last
        MapRef(getTypeRef(keyType, typeParams), getTypeRef(valueType, typeParams))
      case unknown =>
        UnknownTypeRef(unknown)
    }
  }

  @inline private def isSealedUnion(scalaType: Type): Boolean =
    scalaType.typeSymbol.asClass.isTrait && scalaType.typeSymbol.asClass.isSealed && scalaType.typeParams.isEmpty

  @inline private def isCaseClass(scalaType: Type): Boolean =
    scalaType.typeSymbol.isClass && scalaType.typeSymbol.asClass.isCaseClass

  @inline private def isAnyValChild(scalaType: Type): Boolean =
    scalaType <:< typeOf[AnyVal]

  @inline private def isValidMethod(m: MethodSymbol): Boolean =
    m.isPublic && m.isMethod && m.isAccessor && m.isTerm &&
    !m.isConstructor && !m.isStatic && m.returnType != typeOf[Unit] &&
    !m.isImplicit


  private def directKnownSubclasses(tpe: Type): List[Type] = {
    // Workaround for SI-7046: https://issues.scala-lang.org/browse/SI-7046
    val tpeSym = tpe.typeSymbol.asClass

    @annotation.tailrec
    def allSubclasses(path: Traversable[Symbol], subclasses: ListSet[Type]): ListSet[Type] = path.headOption match {
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
        o.companion == NoSymbol && // not a companion object
          o.typeSignature.baseClasses.contains(tpeSym)) =>
        allSubclasses(path.tail, subclasses + o.typeSignature)

      case Some(o: ModuleSymbol) if (
        o.companion == NoSymbol // not a companion object
      ) => allSubclasses(path.tail, subclasses)

      case Some(_) => allSubclasses(path.tail, subclasses)

      case _ => subclasses
    }

    if (tpeSym.isSealed && tpeSym.isAbstract) {
      allSubclasses(tpeSym.owner.typeSignature.decls.sorted, ListSet.empty).toList
    } else List.empty
  }
}
