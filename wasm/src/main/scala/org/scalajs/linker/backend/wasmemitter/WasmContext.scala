package org.scalajs.linker.backend.wasmemitter

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{FieldDef, ParamDef, JSNativeLoadSpec}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.LinkedClass

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._
import org.scalajs.ir.OriginalName

final class WasmContext {
  import WasmContext._

  private val classInfo = mutable.Map[ClassName, ClassInfo]()
  private var reflectiveProxies: Map[MethodName, Int] = null
  private var _itablesLength: Int = -1

  /** Sets the length of the itables arrays, only for use by `Preprocessor`. */
  def setItablesLength(length: Int): Unit =
    _itablesLength = length

  def itablesLength: Int = {
    require(_itablesLength != -1, s"itablesLength was not yet assigned")
    _itablesLength
  }

  private val functionTypes = LinkedHashMap.empty[watpe.FunctionType, wanme.TypeID]
  private val tableFunctionTypes = mutable.HashMap.empty[MethodName, wanme.TypeID]
  private val constantStringGlobals = LinkedHashMap.empty[String, StringData]
  private val closureDataTypes = LinkedHashMap.empty[List[Type], wanme.TypeID]

  val moduleBuilder: ModuleBuilder = {
    new ModuleBuilder(new ModuleBuilder.FunctionTypeProvider {
      def functionTypeToTypeName(sig: watpe.FunctionType): wanme.TypeID = {
        functionTypes.getOrElseUpdate(
          sig, {
            val typeName = genTypeID.forFunction(functionTypes.size)
            moduleBuilder.addRecType(typeName, NoOriginalName, sig)
            typeName
          }
        )
      }
    })
  }

  private var stringPool = new mutable.ArrayBuffer[Byte]()
  private var nextConstantStringIndex: Int = 0
  private var nextClosureDataTypeIndex: Int = 1

  private val _importedModules: mutable.LinkedHashSet[String] =
    new mutable.LinkedHashSet()

  private val _jsPrivateFieldNames: mutable.ListBuffer[FieldName] =
    new mutable.ListBuffer()
  private val _funcDeclarations: mutable.LinkedHashSet[wanme.FunctionID] =
    new mutable.LinkedHashSet()

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  /** Get an index of the itable for the given interface. The itable instance must be placed at the
    * index in the array of itables (whose size is `itablesLength`).
    */
  def getItableIdx(iface: ClassInfo): Int = {
    val idx = iface.itableIdx
    if (idx < 0) throw new IllegalArgumentException(s"Interface $iface is not registed.")
    idx
  }

  def getClassInfoOption(name: ClassName): Option[ClassInfo] =
    classInfo.get(name)

  def getClassInfo(name: ClassName): ClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def inferTypeFromTypeRef(typeRef: TypeRef): Type = typeRef match {
    case PrimRef(tpe) =>
      tpe
    case ClassRef(className) =>
      if (className == ObjectClass || getClassInfo(className).kind.isJSType)
        AnyType
      else
        ClassType(className)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef)
  }

  /** Sets the map of reflexity proxy IDs, only for use by `Preprocessor`. */
  def setReflectiveProxyIDs(proxyIDs: Map[MethodName, Int]): Unit =
    reflectiveProxies = proxyIDs

  /** Retrieves a unique identifier for a reflective proxy with the given name.
    *
    * If no class defines a reflective proxy with the given name, returns `-1`.
    */
  def getReflectiveProxyId(name: MethodName): Int =
    reflectiveProxies.getOrElse(name, -1)

  /** Adds or reuses a function type for a table function.
    *
    * Table function types are part of the main `rectype`, and have names derived from the
    * `methodName`.
    */
  def tableFunctionType(methodName: MethodName): wanme.TypeID = {
    // Project all the names with the same *signatures* onto a normalized `MethodName`
    val normalizedName = MethodName(
      SpecialNames.normalizedSimpleMethodName,
      methodName.paramTypeRefs,
      methodName.resultTypeRef,
      methodName.isReflectiveProxy
    )

    tableFunctionTypes.getOrElseUpdate(
      normalizedName, {
        val typeName = genTypeID.forTableFunctionType(normalizedName)
        val regularParamTyps = normalizedName.paramTypeRefs.map { typeRef =>
          TypeTransformer.transformType(inferTypeFromTypeRef(typeRef))(this)
        }
        val resultTyp =
          TypeTransformer.transformResultType(inferTypeFromTypeRef(normalizedName.resultTypeRef))(
            this
          )
        mainRecType.addSubType(
          typeName,
          NoOriginalName,
          watpe.FunctionType(watpe.RefType.any :: regularParamTyps, resultTyp)
        )
        typeName
      }
    )
  }

  def addConstantStringGlobal(str: String): StringData = {
    constantStringGlobals.get(str) match {
      case Some(data) =>
        data

      case None =>
        val bytes = str.toCharArray.flatMap { char =>
          Array((char & 0xFF).toByte, (char >> 8).toByte)
        }
        val offset = stringPool.size
        val data = StringData(nextConstantStringIndex, offset)
        constantStringGlobals(str) = data

        stringPool ++= bytes
        nextConstantStringIndex += 1
        data
    }
  }

  def getConstantStringInstr(str: String): List[wa.Instr] =
    getConstantStringDataInstr(str) :+ wa.Call(genFunctionID.stringLiteral)

  def getConstantStringDataInstr(str: String): List[wa.I32Const] = {
    val data = addConstantStringGlobal(str)
    List(
      wa.I32Const(data.offset),
      // Assuming that the stringLiteral method will instantiate the
      // constant string from the data section using "array.newData $i16Array ..."
      // The length of the array should be equal to the length of the WTF-16 encoded string
      wa.I32Const(str.length()),
      wa.I32Const(data.constantStringIndex)
    )
  }

  def getClosureDataStructType(captureParamTypes: List[Type]): wanme.TypeID = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[watpe.StructField] =
          for ((tpe, i) <- captureParamTypes.zipWithIndex)
            yield watpe.StructField(
              genFieldID.captureParam(i),
              NoOriginalName,
              TypeTransformer.transformType(tpe)(this),
              isMutable = false
            )
        val structTypeName = genTypeID.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = watpe.StructType(fields)
        moduleBuilder.addRecType(structTypeName, NoOriginalName, structType)
        structTypeName
      }
    )
  }

  def refFuncWithDeclaration(name: wanme.FunctionID): wa.RefFunc = {
    addFuncDeclaration(name)
    wa.RefFunc(name)
  }

  def addExport(exprt: wamod.Export): Unit =
    moduleBuilder.addExport(exprt)

  def addFunction(fun: wamod.Function): Unit =
    moduleBuilder.addFunction(fun)

  def addGlobal(g: wamod.Global): Unit =
    moduleBuilder.addGlobal(g)

  def getImportedModuleGlobal(moduleName: String): wanme.GlobalID = {
    val name = genGlobalID.forImportedModule(moduleName)
    if (_importedModules.add(moduleName)) {
      val origName = OriginalName("import." + moduleName)
      moduleBuilder.addImport(
        wamod.Import(
          "__scalaJSImports",
          moduleName,
          wamod.ImportDesc.Global(name, origName, watpe.RefType.anyref, isMutable = false)
        )
      )
    }
    name
  }

  def allImportedModules: List[String] = _importedModules.toList

  def addFuncDeclaration(name: wanme.FunctionID): Unit =
    _funcDeclarations += name

  def putClassInfo(name: ClassName, info: ClassInfo): Unit =
    classInfo.put(name, info)

  def addJSPrivateFieldName(fieldName: FieldName): Unit =
    _jsPrivateFieldNames += fieldName

  def getFinalStringPool(): (Array[Byte], Int) =
    (stringPool.toArray, nextConstantStringIndex)

  def getAllJSPrivateFieldNames(): List[FieldName] =
    _jsPrivateFieldNames.toList

  def getAllFuncDeclarations(): List[wanme.FunctionID] =
    _funcDeclarations.toList
}

object WasmContext {
  final case class StringData(constantStringIndex: Int, offset: Int)

  final class ClassInfo(
      ctx: WasmContext,
      val name: ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[ParamDef]],
      classConcretePublicMethodNames: List[MethodName],
      val allFieldDefs: List[FieldDef],
      superClass: Option[ClassName],
      ancestors: List[ClassName],
      private var _hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val staticFieldMirrors: Map[FieldName, List[String]],
      private var _itableIdx: Int
  ) {

    /** Does this Scala class implement any interface? */
    val classImplementsAnyInterface =
      if (!kind.isClass && kind != ClassKind.HijackedClass) false
      else ancestors.exists(a => a != name && ctx.getClassInfo(a).isInterface)

    val resolvedMethodInfos: Map[MethodName, ConcreteMethodInfo] = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        val inherited: Map[MethodName, ConcreteMethodInfo] = superClass match {
          case Some(superClass) => ctx.getClassInfo(superClass).resolvedMethodInfos
          case None             => Map.empty
        }

        for (methodName <- classConcretePublicMethodNames)
          inherited.get(methodName).foreach(_.markOverridden())

        classConcretePublicMethodNames.foldLeft(inherited) { (prev, methodName) =>
          prev.updated(methodName, new ConcreteMethodInfo(name, methodName))
        }
      } else {
        Map.empty
      }
    }

    private val methodsCalledDynamically = mutable.HashSet.empty[MethodName]

    /** For a class or interface, its table entries in definition order. */
    private var _tableEntries: List[MethodName] = null

    // See caller in Preprocessor.preprocess
    def setHasInstances(): Unit =
      _hasInstances = true

    def hasInstances: Boolean = _hasInstances

    def setItableIdx(idx: Int): Unit = _itableIdx = idx

    /** Returns the index of this interface's itable in the classes' interface tables.
      */
    def itableIdx: Int = _itableIdx

    private var _specialInstanceTypes: Int = 0

    def addSpecialInstanceType(jsValueType: Int): Unit =
      _specialInstanceTypes |= (1 << jsValueType)

    /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
      *
      * This value is used for instance tests against this class. A JS value `x` is an instance of
      * this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
      * this means testing the following formula:
      *
      * {{{
      * ((1 << jsValueType(x)) & specialInstanceTypes) != 0
      * }}}
      *
      * For example, if this class is `Comparable`, we want the bitset to contain the values for
      * `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
      * and `jl.Double` implement `Comparable`.
      *
      * This field is initialized with 0, and augmented during preprocessing by calls to
      * `addSpecialInstanceType`.
      *
      * This technique is used both for static `isInstanceOf` tests as well as reflective tests
      * through `Class.isInstance`. For the latter, this value is stored in
      * `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
      * generated code.
      *
      * See the `isInstance` and `genInstanceTest` helpers.
      *
      * Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
      * since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
      * `x.isInstanceOf[Int]`, for example.
      */
    def specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
      *
      * This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
      */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface = kind == ClassKind.Interface

    def registerDynamicCall(methodName: MethodName): Unit =
      methodsCalledDynamically += methodName

    def buildMethodTable(): Unit = {
      if (_tableEntries != null)
        throw new IllegalStateException(s"Duplicate call to buildMethodTable() for $name")

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superTableEntries =
            superClass.fold[List[MethodName]](Nil)(sup => ctx.getClassInfo(sup).tableEntries)
          val superTableEntrySet = superTableEntries.toSet

          /* When computing the table entries to add for this class, exclude:
           * - methods that are already in the super class' table entries, and
           * - methods that are effectively final, since they will always be
           *   statically resolved instead of using the table dispatch.
           */
          val newTableEntries = methodsCalledDynamically.toList
            .filter(!superTableEntrySet.contains(_))
            .filterNot(m => resolvedMethodInfos.get(m).exists(_.isEffectivelyFinal))
            .sorted // for stability

          _tableEntries = superTableEntries ::: newTableEntries

        case ClassKind.Interface =>
          _tableEntries = methodsCalledDynamically.toList.sorted // for stability

        case _ =>
          _tableEntries = Nil
      }

      methodsCalledDynamically.clear() // gc
    }

    def tableEntries: List[MethodName] = {
      if (_tableEntries == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableEntries
    }
  }

  final class ConcreteMethodInfo(
      val ownerClass: ClassName,
      val methodName: MethodName
  ) {
    val tableEntryName = genFunctionID.forTableEntry(ownerClass, methodName)

    private var effectivelyFinal: Boolean = true

    private[WasmContext] def markOverridden(): Unit =
      effectivelyFinal = false

    def isEffectivelyFinal: Boolean = effectivelyFinal
  }
}
