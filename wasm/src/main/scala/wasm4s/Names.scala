package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.UTF8String

object Names {
  private def intToUTF8String(value: Int): UTF8String =
    UTF8String(value.toString())

  sealed abstract class WasmName {
    val name: UTF8String
  }

  final case class WasmLocalName private (name: UTF8String) extends WasmName
  object WasmLocalName {
    def fromIR(name: IRNames.LocalName): WasmLocalName =
      new WasmLocalName(name.encoded)

    def fromStr(str: String): WasmLocalName =
      new WasmLocalName(UTF8String(str))

    private val localPrefix = UTF8String("local___")

    def synthetic(id: Int): WasmLocalName =
      new WasmLocalName(localPrefix ++ intToUTF8String(id))

    val newTarget = fromStr("new.target")
    val receiver = fromStr("___<this>")
  }

  final case class WasmLabelName private (name: UTF8String) extends WasmName
  object WasmLabelName {
    def synthetic(id: Int): WasmLabelName = new WasmLabelName(intToUTF8String(id))
  }

  final case class WasmGlobalName private (name: UTF8String) extends WasmName
  object WasmGlobalName {
    private val importedPrefix = UTF8String("imported.")
    private val modinstancePrefix = UTF8String("modinstance.")
    private val jsclassPrefix = UTF8String("jsclass.")
    private val vtablePrefix = UTF8String("vtable.")
    private val vtableClassPrefix = UTF8String("vtable.L")
    private val itableClassPrefix = UTF8String("itable.L")
    private val staticPrefix = UTF8String("static.")
    private val exportPrefix = UTF8String("export.")
    private val jsPrivateFieldPrefix = UTF8String("jspfield.")

    private val dot = UTF8String(".")

    def forImportedModule(moduleName: String): WasmGlobalName =
      new WasmGlobalName(importedPrefix ++ UTF8String(moduleName))

    def forModuleInstance(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(modinstancePrefix ++ className.encoded)

    def forJSClassValue(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(jsclassPrefix ++ className.encoded)

    def forVTable(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(vtableClassPrefix ++ className.encoded)

    def forVTable(typeRef: IRTypes.NonArrayTypeRef): WasmGlobalName = typeRef match {
      case typeRef: IRTypes.PrimRef =>
        new WasmGlobalName(vtablePrefix ++ UTF8String(Array(typeRef.charCode.toByte)))
      case IRTypes.ClassRef(className) =>
        forVTable(className)
    }

    def forITable(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(itableClassPrefix ++ className.encoded)

    def forStaticField(fieldName: IRNames.FieldName): WasmGlobalName =
      new WasmGlobalName(staticPrefix ++ fieldName.className.encoded ++ dot ++ fieldName.simpleName.encoded)

    def forTopLevelExport(exportName: String): WasmGlobalName =
      new WasmGlobalName(exportPrefix ++ UTF8String(exportName))

    def forJSPrivateField(fieldName: IRNames.FieldName): WasmGlobalName =
      new WasmGlobalName(jsPrivateFieldPrefix ++ fieldName.className.encoded ++ dot ++ fieldName.simpleName.encoded)

    val stringLiteralCache: WasmGlobalName =
      new WasmGlobalName(UTF8String("string_literal"))

    val arrayClassITable: WasmGlobalName =
      new WasmGlobalName(UTF8String("itable.A"))

    val lastIDHashCode: WasmGlobalName =
      new WasmGlobalName(UTF8String("lastIDHashCode"))

    val idHashCodeMap: WasmGlobalName =
      new WasmGlobalName(UTF8String("idHashCodeMap"))
  }

  final case class WasmFunctionName private (name: UTF8String) extends WasmName {
    def this(namespace: String, simpleName: String) =
      this(UTF8String(namespace + "#" + simpleName))
  }

  object WasmFunctionName {
    def apply(
        namespace: IRTrees.MemberNamespace,
        clazz: IRNames.ClassName,
        method: IRNames.MethodName
    ): WasmFunctionName = {
      new WasmFunctionName(
        namespaceString(namespace) + "#" + clazz.nameString,
        method.nameString
      )
    }

    private def namespaceString(namespace: IRTrees.MemberNamespace): String = {
      import IRTrees.MemberNamespace._

      // These strings are the same ones that the JS back-end uses
      namespace match {
        case Public            => "f"
        case Private           => "p"
        case PublicStatic      => "s"
        case PrivateStatic     => "ps"
        case Constructor       => "ct"
        case StaticConstructor => "sct"
      }
    }

    def forTableEntry(clazz: IRNames.ClassName, method: IRNames.MethodName): WasmFunctionName =
      new WasmFunctionName("t#" + clazz.nameString, method.nameString)

    def forExport(exportedName: String): WasmFunctionName =
      new WasmFunctionName("export", exportedName)

    def loadModule(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("loadModule", clazz.nameString)
    def newDefault(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("new", clazz.nameString)
    def instanceTest(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("instanceTest", clazz.nameString)
    def clone(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("clone", clazz.nameString)

    def clone(arrayBaseRef: IRTypes.NonArrayTypeRef): WasmFunctionName = {
      val simpleName = arrayBaseRef match {
        case IRTypes.ClassRef(_)  => "O"
        case IRTypes.PrimRef(tpe) => tpe.primRef.charCode.toString()
      }
      new WasmFunctionName("cloneArray", simpleName)
    }

    def isJSClassInstance(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("isJSClassInstance", clazz.nameString)
    def loadJSClass(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("loadJSClass", clazz.nameString)
    def createJSClassOf(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("createJSClassOf", clazz.nameString)
    def preSuperStats(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("preSuperStats", clazz.nameString)
    def superArgs(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("superArgs", clazz.nameString)
    def postSuperStats(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("postSuperStats", clazz.nameString)

    val start = new WasmFunctionName("start", "start")

    private def helper(name: String): WasmFunctionName =
      new WasmFunctionName(UTF8String(name))

    // JS helpers

    val is = helper("is")

    val undef = helper("undef")
    val isUndef = helper("isUndef")

    def box(primRef: IRTypes.PrimRef): WasmFunctionName = helper("b" + primRef.charCode)
    def unbox(primRef: IRTypes.PrimRef): WasmFunctionName = helper("u" + primRef.charCode)
    def unboxOrNull(primRef: IRTypes.PrimRef): WasmFunctionName = helper("uN" + primRef.charCode)
    def typeTest(primRef: IRTypes.PrimRef): WasmFunctionName = helper("t" + primRef.charCode)

    val fmod = helper("fmod")

    val closure = helper("closure")
    val closureThis = helper("closureThis")
    val closureRest = helper("closureRest")
    val closureThisRest = helper("closureThisRest")
    val closureRestNoData = helper("closureRestNoData")

    val emptyString = helper("emptyString")
    val stringLength = helper("stringLength")
    val stringCharAt = helper("stringCharAt")
    val jsValueToString = helper("jsValueToString") // for actual toString() call
    val jsValueToStringForConcat = helper("jsValueToStringForConcat")
    val booleanToString = helper("booleanToString")
    val charToString = helper("charToString")
    val intToString = helper("intToString")
    val longToString = helper("longToString")
    val doubleToString = helper("doubleToString")
    val stringConcat = helper("stringConcat")
    val isString = helper("isString")

    val jsValueType = helper("jsValueType")
    val bigintHashCode = helper("bigintHashCode")
    val symbolDescription = helper("symbolDescription")
    val idHashCodeGet = helper("idHashCodeGet")
    val idHashCodeSet = helper("idHashCodeSet")

    val jsGlobalRefGet = helper("jsGlobalRefGet")
    val jsGlobalRefSet = helper("jsGlobalRefSet")
    val jsGlobalRefTypeof = helper("jsGlobalRefTypeof")
    val jsNewArray = helper("jsNewArray")
    val jsArrayPush = helper("jsArrayPush")
    val jsArraySpreadPush = helper("jsArraySpreadPush")
    val jsNewObject = helper("jsNewObject")
    val jsObjectPush = helper("jsObjectPush")
    val jsSelect = helper("jsSelect")
    val jsSelectSet = helper("jsSelectSet")
    val jsNew = helper("jsNew")
    val jsFunctionApply = helper("jsFunctionApply")
    val jsMethodApply = helper("jsMethodApply")
    val jsImportCall = helper("jsImportCall")
    val jsImportMeta = helper("jsImportMeta")
    val jsDelete = helper("jsDelete")
    val jsForInSimple = helper("jsForInSimple")
    val jsIsTruthy = helper("jsIsTruthy")
    val jsLinkingInfo = helper("jsLinkingInfo")

    val jsUnaryOps: Map[IRTrees.JSUnaryOp.Code, WasmFunctionName] = {
      import IRTrees.JSUnaryOp
      Map(
        JSUnaryOp.+ -> helper("jsUnaryPlus"),
        JSUnaryOp.- -> helper("jsUnaryMinus"),
        JSUnaryOp.~ -> helper("jsUnaryTilde"),
        JSUnaryOp.! -> helper("jsUnaryBang"),
        JSUnaryOp.typeof -> helper("jsUnaryTypeof")
      )
    }

    val jsBinaryOps: Map[IRTrees.JSBinaryOp.Code, WasmFunctionName] = {
      import IRTrees.JSBinaryOp
      Map(
        JSBinaryOp.=== -> helper("jsStrictEquals"),
        JSBinaryOp.!== -> helper("jsNotStrictEquals"),
        JSBinaryOp.+ -> helper("jsPlus"),
        JSBinaryOp.- -> helper("jsMinus"),
        JSBinaryOp.* -> helper("jsTimes"),
        JSBinaryOp./ -> helper("jsDivide"),
        JSBinaryOp.% -> helper("jsModulus"),
        JSBinaryOp.| -> helper("jsBinaryOr"),
        JSBinaryOp.& -> helper("jsBinaryAnd"),
        JSBinaryOp.^ -> helper("jsBinaryXor"),
        JSBinaryOp.<< -> helper("jsShiftLeft"),
        JSBinaryOp.>> -> helper("jsArithmeticShiftRight"),
        JSBinaryOp.>>> -> helper("jsLogicalShiftRight"),
        JSBinaryOp.< -> helper("jsLessThan"),
        JSBinaryOp.<= -> helper("jsLessEqual"),
        JSBinaryOp.> -> helper("jsGreaterThan"),
        JSBinaryOp.>= -> helper("jsGreaterEqual"),
        JSBinaryOp.in -> helper("jsIn"),
        JSBinaryOp.instanceof -> helper("jsInstanceof"),
        JSBinaryOp.** -> helper("jsExponent")
      )
    }

    val newSymbol = helper("newSymbol")
    val createJSClass = helper("createJSClass")
    val createJSClassRest = helper("createJSClassRest")
    val installJSField = helper("installJSField")
    val installJSMethod = helper("installJSMethod")
    val installJSStaticMethod = helper("installJSStaticMethod")
    val installJSProperty = helper("installJSProperty")
    val installJSStaticProperty = helper("installJSStaticProperty")
    val jsSuperGet = helper("jsSuperGet")
    val jsSuperSet = helper("jsSuperSet")
    val jsSuperCall = helper("jsSuperCall")

    // Wasm internal helpers

    val createStringFromData = helper("createStringFromData")
    val stringLiteral = helper("stringLiteral")
    val typeDataName = helper("typeDataName")
    val createClassOf = helper("createClassOf")
    val getClassOf = helper("getClassOf")
    val arrayTypeData = helper("arrayTypeData")
    val isInstance = helper("isInstance")
    val isAssignableFromExternal = helper("isAssignableFromExternal")
    val isAssignableFrom = helper("isAssignableFrom")
    val checkCast = helper("checkCast")
    val getComponentType = helper("getComponentType")
    val newArrayOfThisClass = helper("newArrayOfThisClass")
    val anyGetClass = helper("anyGetClass")
    val newArrayObject = helper("newArrayObject")
    val identityHashCode = helper("identityHashCode")
    val searchReflectiveProxy = helper("searchReflectiveProxy")
  }

  final case class WasmFieldName private (name: UTF8String) extends WasmName
  object WasmFieldName {
    private val methodTabelPrefix = UTF8String("m.")
    private val capturePrefix = UTF8String("c")

    private val dot = UTF8String(".")

    def fromStr(str: String): WasmFieldName =
      new WasmFieldName(UTF8String(str))

    def forClassInstanceField(name: IRNames.FieldName): WasmFieldName =
      new WasmFieldName(name.className.encoded ++ dot ++ name.simpleName.encoded)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def forITable(className: IRNames.ClassName): WasmFieldName =
      new WasmFieldName(className.encoded)

    def forMethodTableEntry(name: IRNames.MethodName): WasmFieldName =
      new WasmFieldName(methodTabelPrefix ++ encodeMethodName(name))

    private def encodeMethodName(methodName: IRNames.MethodName): UTF8String =
      UTF8String(methodName.nameString)

    def captureParam(i: Int): WasmFieldName =
      new WasmFieldName(capturePrefix ++ intToUTF8String(i))

    val vtable = fromStr("vtable")
    val itable = fromStr("itable")
    val itables = fromStr("itables")
    val arrayItem = fromStr("array_item")
    val arrayField = fromStr("array_field")
    val reflectiveProxyField = fromStr("reflective_proxy_field")
    object reflectiveProxy {
      val func_name = fromStr("reflective_proxy_func_name")
      val func_ref = fromStr("reflective_proxy_func_ref")
    }

    // Fields of the typeData structs
    object typeData {

      /** The name data as the 3 arguments to `stringLiteral`.
        *
        * It is only meaningful for primitives and for classes. For array types, they are all 0, as
        * array types compute their `name` from the `name` of their component type.
        */
      val nameOffset = fromStr("nameOffset")

      /** See `nameOffset`. */
      val nameSize = fromStr("nameSize")

      /** See `nameOffset`. */
      val nameStringIndex = fromStr("nameStringIndex")

      /** The kind of type data, an `i32`.
        *
        * Possible values are the the `KindX` constants in `EmbeddedConstants`.
        */
      val kind = fromStr("kind")

      /** A bitset of special (primitive) instance types that are instances of this type, an `i32`.
        *
        * From 0 to 5, the bits correspond to the values returned by the helper `jsValueType`. In
        * addition, bits 6 and 7 represent `char` and `long`, respectively.
        */
      val specialInstanceTypes = fromStr("specialInstanceTypes")

      /** Array of the strict ancestor classes of this class.
        *
        * This is `null` for primitive and array types. For all other types, including JS types, it
        * contains an array of the typeData of their ancestors that:
        *
        *   - are not themselves (hence the *strict* ancestors),
        *   - have typeData to begin with.
        */
      val strictAncestors = fromStr("strictAncestors")

      /** The typeData of a component of this array type, or `null` if this is not an array type.
        *
        * For example:
        *   - the `componentType` for class `Foo` is `null`,
        *   - the `componentType` for the array type `Array[Foo]` is the `typeData` of `Foo`.
        */
      val componentType = fromStr("componentType")

      /** The name as nullable string (`anyref`), lazily initialized from the nameData.
        *
        * This field is initialized by the `typeDataName` helper.
        *
        * The contents of this value is specified by `java.lang.Class.getName()`. In particular, for
        * array types, it obeys the following rules:
        *
        *   - `Array[prim]` where `prim` is a one of the primitive types with `charCode` `X` is
        *     `"[X"`, for example, `"[I"` for `Array[Int]`.
        *   - `Array[pack.Cls]` where `Cls` is a class is `"[Lpack.Cls;"`.
        *   - `Array[nestedArray]` where `nestedArray` is an array type with name `nested` is
        *     `"[nested"`, for example `"[[I"` for `Array[Array[Int]]` and `"[[Ljava.lang.String;"`
        *     for `Array[Array[String]]`.
        */
      val name = fromStr("name")

      /** The `classOf` value, a nullable `java.lang.Class`, lazily initialized from this typeData.
        *
        * This field is initialized by the `createClassOf` helper.
        */
      val classOfValue = fromStr("classOf")

      /** The typeData/vtable of an array of this type, a nullable `typeData`, lazily initialized.
        *
        * This field is initialized by the `arrayTypeData` helper.
        *
        * For example, once initialized,
        *   - in the `typeData` of class `Foo`, it contains the `typeData` of `Array[Foo]`,
        *   - in the `typeData` of `Array[Int]`, it contains the `typeData` of `Array[Array[Int]]`.
        */
      val arrayOf = fromStr("arrayOf")

      /** The function to clone the object of this type, a nullable function reference. This field
        * is instantiated only with the classes that implement java.lang.Cloneable.
        */
      val cloneFunction = fromStr("clone")

      /** `isInstance` func ref for top-level JS classes. */
      val isJSClassInstance = fromStr("isJSClassInstance")

      /** The reflective proxies in this type, used for reflective call on the class at runtime.
        * This field contains an array of reflective proxy structs, where each struct contains the
        * ID of the reflective proxy and a reference to the actual method implementation. Reflective
        * call site should walk through the array to look up a method to call.
        *
        * See `genSearchReflectivePRoxy` in `HelperFunctions`
        */
      val reflectiveProxies = fromStr("reflectiveProxies")
    }
  }

  final case class WasmFieldIdx(value: Int)

  object WasmFieldIdx {
    val vtable = WasmFieldIdx(0)
    val itables = WasmFieldIdx(1)
    val uniqueRegularField = WasmFieldIdx(2)

    object typeData {
      val nameOffsetIdx = WasmFieldIdx(0)
      val nameSizeIdx = WasmFieldIdx(1)
      val nameStringIndexIdx = WasmFieldIdx(2)
      val kindIdx = WasmFieldIdx(3)
      val specialInstanceTypesIdx = WasmFieldIdx(4)
      val strictAncestorsIdx = WasmFieldIdx(5)
      val componentTypeIdx = WasmFieldIdx(6)
      val nameIdx = WasmFieldIdx(7)
      val classOfIdx = WasmFieldIdx(8)
      val arrayOfIdx = WasmFieldIdx(9)
      val cloneFunctionIdx = WasmFieldIdx(10)
      val isJSClassInstanceIdx = WasmFieldIdx(11)
      val reflectiveProxiesIdx = WasmFieldIdx(12)
    }

    object reflectiveProxy {
      val nameIdx = WasmFieldIdx(0)
      val funcIdx = WasmFieldIdx(1)
    }
  }

  // GC types ====
  final case class WasmTypeName private (name: UTF8String) extends WasmName
  object WasmTypeName {
    def fromStr(str: String): WasmTypeName =
      new WasmTypeName(UTF8String(str))

    object WasmStructTypeName {
      private val classPrefix = UTF8String("c.L")
      private val captureDataPrefix = UTF8String("captureData.")
      private val vtablePrefix = UTF8String("v.")
      private val itablePrefix = UTF8String("i.")

      def forClass(name: IRNames.ClassName): WasmTypeName =
        new WasmTypeName(classPrefix ++ name.encoded)

      def captureData(index: Int): WasmTypeName =
        new WasmTypeName(captureDataPrefix ++ intToUTF8String(index))

      val typeData = fromStr("typeData")
      val reflectiveProxy = fromStr("reflective_proxy")

      // Array types -- they extend j.l.Object
      val BooleanArray = fromStr("c.AZ")
      val CharArray = fromStr("c.AC")
      val ByteArray = fromStr("c.AB")
      val ShortArray = fromStr("c.AS")
      val IntArray = fromStr("c.AI")
      val LongArray = fromStr("c.AJ")
      val FloatArray = fromStr("c.AF")
      val DoubleArray = fromStr("c.AD")
      val ObjectArray = fromStr("c.AO")

      def forArrayClass(arrayTypeRef: IRTypes.ArrayTypeRef): WasmTypeName = {
        if (arrayTypeRef.dimensions > 1) {
          ObjectArray
        } else {
          arrayTypeRef.base match {
            case IRTypes.BooleanRef => BooleanArray
            case IRTypes.CharRef    => CharArray
            case IRTypes.ByteRef    => ByteArray
            case IRTypes.ShortRef   => ShortArray
            case IRTypes.IntRef     => IntArray
            case IRTypes.LongRef    => LongArray
            case IRTypes.FloatRef   => FloatArray
            case IRTypes.DoubleRef  => DoubleArray
            case _                  => ObjectArray
          }
        }
      }

      def forVTable(className: IRNames.ClassName): WasmTypeName =
        new WasmTypeName(vtablePrefix ++ className.encoded)

      val ObjectVTable: WasmTypeName = forVTable(IRNames.ObjectClass)

      def forITable(className: IRNames.ClassName): WasmTypeName =
        new WasmTypeName(itablePrefix ++ className.encoded)
    }

    object WasmArrayTypeName {
      val typeDataArray = fromStr("a.typeDataArray")
      val itables = fromStr("a.itable")
      val reflectiveProxies = fromStr("a.reflectiveProxies")

      // primitive array types, underlying the Array[T] classes
      val i8Array = fromStr("a.i8Array")
      val i16Array = fromStr("a.i16Array")
      val i32Array = fromStr("a.i32Array")
      val i64Array = fromStr("a.i64Array")
      val f32Array = fromStr("a.f32Array")
      val f64Array = fromStr("a.f64Array")
      val anyArray = fromStr("a.anyArray")

      def underlyingOf(arrayTypeRef: IRTypes.ArrayTypeRef): WasmTypeName = {
        if (arrayTypeRef.dimensions > 1) {
          anyArray
        } else {
          arrayTypeRef.base match {
            case IRTypes.BooleanRef => i8Array
            case IRTypes.CharRef    => i16Array
            case IRTypes.ByteRef    => i8Array
            case IRTypes.ShortRef   => i16Array
            case IRTypes.IntRef     => i32Array
            case IRTypes.LongRef    => i64Array
            case IRTypes.FloatRef   => f32Array
            case IRTypes.DoubleRef  => f64Array
            case _                  => anyArray
          }
        }
      }
    }

    object WasmFunctionTypeName {
      def apply(idx: Int): WasmTypeName = fromStr(s"f.$idx")

      def rec(idx: Int): WasmTypeName = fromStr(s"recf.$idx")
    }

  }

  final case class WasmTagName private (name: UTF8String) extends WasmName
  object WasmTagName {
    def fromStr(str: String): WasmTagName = new WasmTagName(UTF8String(str))
  }

  final case class WasmDataName private (name: UTF8String) extends WasmName
  object WasmDataName {
    val string = WasmDataName(UTF8String("string"))
  }

  final case class WasmExportName private (name: UTF8String) extends WasmName
  object WasmExportName {
    def fromStr(str: String): WasmExportName = new WasmExportName(UTF8String(str))
  }

}
