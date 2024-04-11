package wasm
package ir2wasm

import wasm4s._
import wasm4s.WasmContext._
import wasm4s.Names._
import wasm4s.Types._
import wasm4s.WasmInstr._
import TypeTransformer._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{ClassKind, Position}

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import collection.mutable
import java.awt.Window.Type
import _root_.wasm4s.Defaults

import EmbeddedConstants._

class WasmBuilder {
  // val module = new WasmModule()

  def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import WasmFieldName.typeData._

    val primRefsWithTypeData = List(
      IRTypes.VoidRef -> KindVoid,
      IRTypes.BooleanRef -> KindBoolean,
      IRTypes.CharRef -> KindChar,
      IRTypes.ByteRef -> KindByte,
      IRTypes.ShortRef -> KindShort,
      IRTypes.IntRef -> KindInt,
      IRTypes.LongRef -> KindLong,
      IRTypes.FloatRef -> KindFloat,
      IRTypes.DoubleRef -> KindDouble
    )

    for ((primRef, kind) <- primRefsWithTypeData) {
      val typeDataFieldValues = genTypeDataFieldValues(kind, specialInstanceTypes = 0, primRef, Nil)
      val typeDataGlobal =
        genTypeDataGlobal(primRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }
  }

  def transformClassDef(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    val classInfo = ctx.getClassInfo(clazz.className)

    if (!clazz.kind.isClass && classInfo.hasRuntimeTypeInfo) {
      // Gen typeData -- for classes, we do it as part of the vtable generation
      val typeRef = IRTypes.ClassRef(clazz.className)
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val typeDataGlobal =
        genTypeDataGlobal(typeRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }

    // Declare static fields
    for {
      field @ IRTrees.FieldDef(flags, name, _, ftpe) <- clazz.fields
      if flags.namespace.isStatic
    } {
      val typ = transformType(ftpe)
      val global = WasmGlobal(
        WasmGlobalName.forStaticField(name.name),
        typ,
        WasmExpr(List(Defaults.defaultValue(typ))),
        isMutable = true
      )
      ctx.addGlobal(global)
    }

    // Generate method implementations
    for (method <- clazz.methods) {
      if (method.body.isDefined)
        genFunction(clazz, method)
    }

    clazz.kind match {
      case ClassKind.ModuleClass   => transformModuleClass(clazz)
      case ClassKind.Class         => transformClass(clazz)
      case ClassKind.HijackedClass => transformHijackedClass(clazz)
      case ClassKind.Interface     => transformInterface(clazz)

      case ClassKind.JSClass | ClassKind.JSModuleClass =>
        transformJSClass(clazz)
      case ClassKind.AbstractJSType | ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
        () // nothing to do
    }
  }

  def genArrayClasses()(implicit ctx: WasmContext): Unit = {
    import WasmTypeName.WasmStructTypeName

    // The vtable type is always the same as j.l.Object
    val vtableTypeName = WasmStructTypeName.ObjectVTable
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      WasmRefType(vtableTypeName),
      isMutable = false
    )

    val objectRef = IRTypes.ClassRef(IRNames.ObjectClass)

    val typeRefsWithArrays: List[(IRTypes.NonArrayTypeRef, WasmStructTypeName, WasmArrayType)] =
      List(
        (IRTypes.BooleanRef, WasmStructTypeName.BooleanArray, WasmArrayType.i8Array),
        (IRTypes.CharRef, WasmStructTypeName.CharArray, WasmArrayType.i16Array),
        (IRTypes.ByteRef, WasmStructTypeName.ByteArray, WasmArrayType.i8Array),
        (IRTypes.ShortRef, WasmStructTypeName.ShortArray, WasmArrayType.i16Array),
        (IRTypes.IntRef, WasmStructTypeName.IntArray, WasmArrayType.i32Array),
        (IRTypes.LongRef, WasmStructTypeName.LongArray, WasmArrayType.i64Array),
        (IRTypes.FloatRef, WasmStructTypeName.FloatArray, WasmArrayType.f32Array),
        (IRTypes.DoubleRef, WasmStructTypeName.DoubleArray, WasmArrayType.f64Array),
        (objectRef, WasmStructTypeName.ObjectArray, WasmArrayType.anyArray)
      )

    for ((baseRef, structTypeName, underlyingArrayType) <- typeRefsWithArrays) {
      val underlyingArrayField = WasmStructField(
        WasmFieldName.arrayField,
        WasmRefType(underlyingArrayType.name),
        isMutable = false
      )

      val structType = WasmStructType(
        structTypeName,
        List(vtableField, WasmStructField.itables, underlyingArrayField),
        Some(Names.WasmTypeName.WasmStructTypeName.forClass(IRNames.ObjectClass))
      )
      ctx.addGCType(structType)

      HelperFunctions.genArrayCloneFunction(IRTypes.ArrayTypeRef(baseRef, 1))
    }

    genArrayClassItable()
  }

  def transformTopLevelExport(
      topLevelExport: LinkedTopLevelExport
  )(implicit ctx: WasmContext): Unit = {
    topLevelExport.tree match {
      case d: IRTrees.TopLevelJSClassExportDef => ???
      case d: IRTrees.TopLevelModuleExportDef  => ???
      case d: IRTrees.TopLevelMethodExportDef  => transformTopLevelMethodExportDef(d)
      case d: IRTrees.TopLevelFieldExportDef   => transformTopLevelFieldExportDef(d)
    }
  }

  private def genTypeDataFieldValues(clazz: LinkedClass, vtableElems: List[WasmFunctionInfo])(
      implicit ctx: WasmContext
  ): List[WasmInstr] = {
    import WasmFieldName.typeData._

    val className = clazz.className
    val classInfo = ctx.getClassInfo(className)

    /* See the `isInstance` helper. `specialInstanceTypes` is a bitset of the
     * `jsValueType`s corresponding to hijacked classes that extend this class.
     * For example, if this class is `Comparable`, we want the bitset to contain
     * the values for `boolean`, `string` and `number` (but not `undefined`),
     * because `jl.Boolean`, `jl.String` and `jl.Double` implement `Comparable`.
     *
     * When testing whether a `value` is a `Comparable`, `isInstance` will
     * compute the `jsValueType(value)` and test whether it is part of the bit
     * set, using `((1 << jsValueType(value)) & specialInstanceTypes) != 0`.
     */
    val specialInstanceTypes = {
      if (!classInfo.isAncestorOfHijackedClass) {
        // fast path
        0
      } else {
        var bits = 0
        if (ctx.getClassInfo(IRNames.BoxedBooleanClass).ancestors.contains(className))
          bits |= ((1 << JSValueTypeFalse) | (1 << JSValueTypeTrue))
        if (ctx.getClassInfo(IRNames.BoxedStringClass).ancestors.contains(className))
          bits |= (1 << JSValueTypeString)
        if (ctx.getClassInfo(IRNames.BoxedDoubleClass).ancestors.contains(className))
          bits |= (1 << JSValueTypeNumber)
        if (ctx.getClassInfo(IRNames.BoxedUnitClass).ancestors.contains(className))
          bits |= (1 << JSValueTypeUndefined)
        bits
      }
    }

    val kind = clazz.className match {
      case IRNames.ObjectClass         => KindObject
      case IRNames.BoxedUnitClass      => KindBoxedUnit
      case IRNames.BoxedBooleanClass   => KindBoxedBoolean
      case IRNames.BoxedCharacterClass => KindBoxedCharacter
      case IRNames.BoxedByteClass      => KindBoxedByte
      case IRNames.BoxedShortClass     => KindBoxedShort
      case IRNames.BoxedIntegerClass   => KindBoxedInteger
      case IRNames.BoxedLongClass      => KindBoxedLong
      case IRNames.BoxedFloatClass     => KindBoxedFloat
      case IRNames.BoxedDoubleClass    => KindBoxedDouble
      case IRNames.BoxedStringClass    => KindBoxedString

      case _ =>
        clazz.kind match {
          case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass => KindClass
          case ClassKind.Interface                                               => KindInterface
          case _                                                                 => KindJSType
        }
    }

    genTypeDataFieldValues(
      kind,
      specialInstanceTypes,
      IRTypes.ClassRef(clazz.className),
      vtableElems
    )
  }

  private def genTypeDataFieldValues(
      kind: Int,
      specialInstanceTypes: Int,
      typeRef: IRTypes.NonArrayTypeRef,
      vtableElems: List[WasmFunctionInfo]
  )(implicit
      ctx: WasmContext
  ): List[WasmInstr] = {
    val nameStr = typeRef match {
      case typeRef: IRTypes.PrimRef    => typeRef.displayName
      case IRTypes.ClassRef(className) => className.nameString
    }

    val nameDataValueItems = nameStr.toList.map(c => I32_CONST(c.toInt))
    val nameDataValueArrayNew =
      ARRAY_NEW_FIXED(
        WasmTypeName.WasmArrayTypeName.i16Array,
        nameDataValueItems.size
      )
    val nameDataValue: List[WasmInstr] = nameDataValueItems :+ nameDataValueArrayNew

    val strictAncestorsValue: List[WasmInstr] = {
      typeRef match {
        case IRTypes.ClassRef(className) =>
          val ancestors = ctx.getClassInfo(className).ancestors

          // By spec, the first element of `ancestors` is always the class itself
          assert(
            ancestors.headOption.contains(className),
            s"The ancestors of ${className.nameString} do not start with itself: $ancestors"
          )
          val strictAncestors = ancestors.tail

          val elems = for {
            ancestor <- strictAncestors
            if ctx.getClassInfo(ancestor).hasRuntimeTypeInfo
          } yield {
            GLOBAL_GET(WasmGlobalName.forVTable(ancestor))
          }
          elems :+ ARRAY_NEW_FIXED(
            WasmTypeName.WasmArrayTypeName.typeDataArray,
            elems.size
          )
        case _ =>
          REF_NULL(WasmHeapType.None) :: Nil
      }
    }

    val cloneFunction = {
      val nullref =
        REF_NULL(WasmHeapType(ctx.cloneFunctionTypeName))
      typeRef match {
        case IRTypes.ClassRef(className) =>
          val classInfo = ctx.getClassInfo(className)
          // If the class is concrete and implements the `java.lang.Cloneable`,
          // `HelperFunctions.genCloneFunction` should've generated the clone function
          if (!classInfo.isAbstract && classInfo.ancestors.contains(IRNames.CloneableClass))
            REF_FUNC(WasmFunctionName.clone(className))
          else nullref
        case _ => nullref
      }
    }

    val reflectiveProxies: List[WasmInstr] = {
      val proxies = vtableElems.filter(_.isReflectiveProxy)
      proxies.flatMap { method =>
        val proxyId = ctx.getReflectiveProxyId(method.name.simpleName)
        List(
          I32_CONST(proxyId),
          REF_FUNC(method.name),
          STRUCT_NEW(Names.WasmTypeName.WasmStructTypeName.reflectiveProxy)
        )
      } :+ ARRAY_NEW_FIXED(Names.WasmTypeName.WasmArrayTypeName.reflectiveProxies, proxies.size)
    }

    nameDataValue :::
      List(
        // kind
        I32_CONST(kind),
        // specialInstanceTypes
        I32_CONST(specialInstanceTypes)
      ) ::: (
        // strictAncestors
        strictAncestorsValue
      ) :::
      List(
        // componentType - always `null` since this method is not used for array types
        REF_NULL(WasmHeapType(WasmTypeName.WasmStructTypeName.typeData)),
        // name - initially `null`; filled in by the `typeDataName` helper
        REF_NULL(WasmHeapType.Any),
        // the classOf instance - initially `null`; filled in by the `createClassOf` helper
        REF_NULL(WasmHeapType.ClassType),
        // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
        REF_NULL(WasmHeapType(WasmTypeName.WasmStructTypeName.ObjectVTable)),
        // clonefFunction - will be invoked from `clone()` method invokaion on the class
        cloneFunction
      ) :::
      // reflective proxies - used to reflective call on the class at runtime.
      // Generated instructions create an array of reflective proxy structs, where each struct
      // contains the ID of the reflective proxy and a reference to the actual method implementation.
      reflectiveProxies
  }

  private def genTypeDataGlobal(
      typeRef: IRTypes.NonArrayTypeRef,
      typeDataType: WasmStructType,
      typeDataFieldValues: List[WasmInstr],
      vtableElems: List[REF_FUNC]
  )(implicit ctx: WasmContext): WasmGlobal = {
    val instrs: List[WasmInstr] =
      typeDataFieldValues ::: vtableElems ::: STRUCT_NEW(typeDataType.name) :: Nil
    WasmGlobal(
      WasmGlobalName.forVTable(typeRef),
      WasmRefType(typeDataType.name),
      WasmExpr(instrs),
      isMutable = false
    )
  }

  /** @return
    *   Optionally returns the generated struct type for this class. If the given LinkedClass is an
    *   abstract class, returns None
    */
  private def transformClassCommon(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): WasmStructType = {
    val className = clazz.name.name
    val typeRef = IRTypes.ClassRef(className)
    val classInfo = ctx.getClassInfo(className)

    // generate vtable type, this should be done for both abstract and concrete classes
    val vtable = ctx.calculateVtableType(className)
    val vtableType = genVTableType(clazz, vtable.functions)
    ctx.addGCType(vtableType)

    val isAbstractClass = !clazz.hasDirectInstances

    // we should't generate global vtable for abstract class because
    // - Can't generate Global vtable because we can't fill the slot for abstract methods
    // - We won't access vtable for abstract classes since we can't instantiate abstract classes, there's no point generating
    //
    // When we don't generate a vtable, we still generate the typeData

    if (!isAbstractClass) {
      // Generate an actual vtable
      val functions = ctx.calculateGlobalVTable(className)
      val typeDataFieldValues = genTypeDataFieldValues(clazz, functions)
      val vtableElems = functions.map(method => WasmInstr.REF_FUNC(method.name))
      val globalVTable = genTypeDataGlobal(typeRef, vtableType, typeDataFieldValues, vtableElems)
      ctx.addGlobal(globalVTable)
      genGlobalClassItable(clazz)
    } else if (classInfo.hasRuntimeTypeInfo) {
      // Only generate typeData
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val globalTypeData =
        genTypeDataGlobal(typeRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(globalTypeData)
    }

    // Declare the struct type for the class
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      WasmRefType(vtableType.name),
      isMutable = false
    )
    val fields = classInfo.allFieldDefs.map(transformField)
    val structType = WasmStructType(
      Names.WasmTypeName.WasmStructTypeName.forClass(clazz.name.name),
      vtableField +: WasmStructField.itables +: fields,
      clazz.superClass.map(s => Names.WasmTypeName.WasmStructTypeName.forClass(s.name))
    )
    ctx.addGCType(structType)

    // Define the `new` function, unless the class is abstract
    if (!isAbstractClass) HelperFunctions.genNewDefault(clazz)
    structType
  }

  private def genVTableType(clazz: LinkedClass, functions: List[WasmFunctionInfo])(implicit
      ctx: WasmContext
  ): WasmStructType = {
    val vtableFields =
      functions.map { method =>
        WasmStructField(
          Names.WasmFieldName.forMethodTableEntry(method.name),
          WasmRefType.nullable(method.toWasmFunctionType().name),
          isMutable = false
        )
      }
    val superType = clazz.superClass match {
      case None    => WasmTypeName.WasmStructTypeName.typeData
      case Some(s) => WasmTypeName.WasmStructTypeName.forVTable(s.name)
    }
    WasmStructType(
      Names.WasmTypeName.WasmStructTypeName.forVTable(clazz.name.name),
      WasmStructType.typeData.fields ::: vtableFields,
      Some(superType)
    )
  }

  private def genLoadModuleFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.ModuleClass)
    val ctor = clazz.methods
      .find(_.methodName.isConstructor)
      .getOrElse(throw new Error(s"Module class should have a constructor, ${clazz.name}"))
    val typeName = WasmTypeName.WasmStructTypeName.forClass(clazz.name.name)
    val globalInstanceName = WasmGlobalName.forModuleInstance(clazz.name.name)

    val ctorName = WasmFunctionName(
      ctor.flags.namespace,
      clazz.name.name,
      ctor.name.name
    )

    val body = List(
      // global.get $module_name
      // ref.if_null
      //   ref.null $module_type
      //   call $module_init ;; should set to global
      // end
      // global.get $module_name
      GLOBAL_GET(globalInstanceName), // [rt]
      REF_IS_NULL, // [rt] -> [i32] (bool)
      IF(BlockType.ValueType()),
      CALL(WasmFunctionName.newDefault(clazz.name.name)),
      GLOBAL_SET(globalInstanceName),
      GLOBAL_GET(globalInstanceName),
      CALL(ctorName),
      // ELSE,
      END,
      GLOBAL_GET(globalInstanceName) // [rt]
    )

    val sig =
      WasmFunctionSignature(Nil, List(WasmRefType.nullable(typeName)))
    val loadModuleTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.loadModule(clazz.name.name),
      WasmFunctionType(loadModuleTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    ctx.addFunction(func)
  }

  /** Generate global instance of the class itable. Their init value will be an array of null refs
    * of size = number of interfaces. They will be initialized in start function
    */
  private def genGlobalClassItable(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): Unit = {
    val info = ctx.getClassInfo(clazz.className)
    val implementsAnyInterface = info.ancestors.exists(a => ctx.getClassInfo(a).isInterface)
    if (implementsAnyInterface) {
      val globalName = WasmGlobalName.forITable(clazz.className)
      ctx.addGlobalITable(clazz.className, genITableGlobal(globalName))
    }
  }

  private def genArrayClassItable()(implicit ctx: WasmContext): Unit =
    ctx.addGlobal(genITableGlobal(WasmGlobalName.arrayClassITable))

  private def genITableGlobal(name: WasmGlobalName)(implicit ctx: WasmContext): WasmGlobal = {
    val itablesInit = List(
      I32_CONST(ctx.itablesLength),
      ARRAY_NEW_DEFAULT(WasmArrayType.itables.name)
    )
    WasmGlobal(
      name,
      WasmRefType(WasmArrayType.itables.name),
      init = WasmExpr(itablesInit),
      isMutable = false
    )
  }

  private def transformClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Class)
    transformClassCommon(clazz)
  }

  private def transformHijackedClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    // nothing to do
    ()
  }

  private def transformInterface(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Interface)
    // gen itable type
    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(clazz.className)
    val itableType = WasmStructType(
      Names.WasmTypeName.WasmStructTypeName.forITable(className),
      classInfo.methods.map { m =>
        WasmStructField(
          Names.WasmFieldName(m.name.simpleName),
          WasmRefType.nullable(m.toWasmFunctionType().name),
          isMutable = false
        )
      },
      None
    )
    ctx.addGCType(itableType)
    // typeName
    // genITable
    // generateVTable()
  }

  private def transformModuleClass(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    val structType = transformClassCommon(clazz)
    val heapType = WasmHeapType(structType.name)

    if (clazz.hasInstances) {
      // global instance
      // (global name (ref null type))
      val global = WasmGlobal(
        Names.WasmGlobalName.forModuleInstance(clazz.name.name),
        WasmRefType.nullable(heapType),
        WasmExpr(List(REF_NULL(heapType))),
        isMutable = true
      )
      ctx.addGlobal(global)

      genLoadModuleFunc(clazz)
    }
  }

  private def transformJSClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind.isJSClass)

    // Define the globals holding the Symbols of private fields
    for (fieldDef <- clazz.fields) {
      fieldDef match {
        case IRTrees.FieldDef(flags, name, _, _) if !flags.namespace.isStatic =>
          ctx.addGlobal(
            WasmGlobal(
              WasmGlobalName.forJSPrivateField(name.name),
              WasmRefType.anyref,
              WasmExpr(List(REF_NULL(WasmHeapType.Any))),
              isMutable = true
            )
          )
          ctx.addJSPrivateFieldName(name.name)
        case _ =>
          ()
      }
    }

    if (clazz.hasInstances) {
      genCreateJSClassFunction(clazz)

      if (clazz.jsClassCaptures.isEmpty)
        genLoadJSClassFunction(clazz)

      if (clazz.kind == ClassKind.JSModuleClass)
        genLoadJSModuleFunction(clazz)
    }
  }

  private def genCreateJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val noPos: Position = Position.NoPosition

    val jsClassCaptures = clazz.jsClassCaptures.getOrElse(Nil)

    /* We need to decompose the body of the constructor into 3 closures.
     * Given an IR constructor of the form
     *   constructor(...params) {
     *     preSuperStats;
     *     super(...superArgs);
     *     postSuperStats;
     *   }
     * We will create closures for `preSuperStats`, `superArgs` and `postSuperStats`.
     *
     * There is one huge catch: `preSuperStats` can declare `VarDef`s at its top-level,
     * and those vars are still visible inside `superArgs` and `postSuperStats`.
     * The `preSuperStats` must therefore return a struct with the values of its
     * declared vars, which will be given as an additional argument to `superArgs`
     * and `postSuperStats`. We call that struct the `preSuperEnv`.
     *
     * In the future, we should optimize `preSuperEnv` to only store locals that
     * are still used by `superArgs` and/or `postSuperArgs`.
     */

    val ctor = clazz.jsConstructorDef.get
    val allCtorParams = ctor.args ::: ctor.restParam.toList
    val ctorBody = ctor.body

    // Compute the pre-super environment
    val preSuperDecls = ctorBody.beforeSuper.collect { case varDef: IRTrees.VarDef =>
      varDef
    }

    // Build the `preSuperStats` function
    val preSuperStatsFun = {
      val preSuperEnvStructType = ctx.getClosureDataStructType(preSuperDecls.map(_.vtpe))
      val preSuperEnvTyp = WasmRefType(preSuperEnvStructType.name)

      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.preSuperStats(clazz.className),
        Some(jsClassCaptures),
        preSuperVarDefs = None,
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(preSuperEnvTyp)
      )

      import fctx.instrs

      WasmExpressionBuilder.generateBlockStats(ctorBody.beforeSuper) {
        // Build and return the preSuperEnv struct
        for (varDef <- preSuperDecls)
          instrs += LOCAL_GET(fctx.lookupLocalAssertLocalStorage(varDef.name.name))
        instrs += STRUCT_NEW(preSuperEnvStructType.name)
      }

      fctx.buildAndAddToContext()
    }

    // Build the `superArgs` function
    val superArgsFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.superArgs(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(WasmRefType.anyref) // a js.Array
      )

      WasmExpressionBuilder.generateIRBody(
        IRTrees.JSArrayConstr(ctorBody.superCall.args),
        IRTypes.AnyType
      )

      fctx.buildAndAddToContext()
    }

    // Build the `postSuperStats` function
    val postSuperStatsFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.postSuperStats(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = Some(WasmRefType.anyref),
        allCtorParams,
        List(WasmRefType.anyref)
      )

      import fctx.instrs

      // Create fields
      for (fieldDef <- clazz.fields if !fieldDef.flags.namespace.isStatic) {
        // Load instance
        instrs += LOCAL_GET(fctx.receiverStorage.idx)

        // Load name
        fieldDef match {
          case IRTrees.FieldDef(_, name, _, _) =>
            instrs += GLOBAL_GET(WasmGlobalName.forJSPrivateField(name.name))
          case IRTrees.JSFieldDef(_, nameTree, _) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)
        }

        // Generate boxed representation of the zero of the field
        WasmExpressionBuilder.generateIRBody(IRTypes.zeroOf(fieldDef.ftpe), IRTypes.AnyType)

        instrs += CALL(WasmFunctionName.installJSField)
      }

      WasmExpressionBuilder.generateIRBody(
        IRTrees.Block(ctorBody.afterSuper),
        IRTypes.AnyType
      )

      fctx.buildAndAddToContext()
    }

    // Build the actual `createJSClass` function
    val createJSClassFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.createJSClassOf(clazz.className),
        None,
        None,
        jsClassCaptures,
        List(WasmRefType.any)
      )

      import fctx.instrs

      // Bundle class captures in a capture data struct -- leave it on the stack for createJSClass
      val dataStructType = ctx.getClosureDataStructType(jsClassCaptures.map(_.ptpe))
      val dataStructLocal = fctx.addLocal(
        "__classCaptures",
        WasmRefType(dataStructType.name)
      )
      for (cc <- jsClassCaptures)
        instrs += LOCAL_GET(fctx.lookupLocalAssertLocalStorage(cc.name.name))
      instrs += STRUCT_NEW(dataStructType.name)
      instrs += LOCAL_TEE(dataStructLocal)

      /* Load super constructor; specified by
       * https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-classdef-runtime-semantics-evaluation
       * - if `jsSuperClass` is defined, evaluate it;
       * - otherwise evaluate `LoadJSConstructor` of the declared superClass.
       */
      val jsSuperClassTree = clazz.jsSuperClass.getOrElse {
        IRTrees.LoadJSConstructor(clazz.superClass.get.name)
      }
      WasmExpressionBuilder.generateIRBody(jsSuperClassTree, IRTypes.AnyType)

      // Load the references to the 3 functions that make up the constructor
      instrs += ctx.refFuncWithDeclaration(preSuperStatsFun.name)
      instrs += ctx.refFuncWithDeclaration(superArgsFun.name)
      instrs += ctx.refFuncWithDeclaration(postSuperStatsFun.name)

      // Call the createJSClass helper to bundle everything
      if (ctor.restParam.isDefined) {
        instrs += I32_CONST(ctor.args.size) // number of fixed params
        instrs += CALL(WasmFunctionName.createJSClassRest)
      } else {
        instrs += CALL(WasmFunctionName.createJSClass)
      }

      // Store the result, locally and possibly in the global cache
      val jsClassLocal = fctx.addLocal("__jsClass", WasmRefType.any)
      if (clazz.jsClassCaptures.isEmpty) {
        // Static JS class with a global cache
        instrs += LOCAL_TEE(jsClassLocal)
        instrs += GLOBAL_SET(WasmGlobalName.forJSClassValue(clazz.className))
      } else {
        // Local or inner JS class, which is new every time
        instrs += LOCAL_SET(jsClassLocal)
      }

      // Install methods and properties
      for (methodOrProp <- clazz.exportedMembers) {
        val isStatic = methodOrProp.flags.namespace.isStatic
        instrs += LOCAL_GET(dataStructLocal)
        instrs += LOCAL_GET(jsClassLocal)

        val receiverTyp = if (isStatic) None else Some(WasmRefType.anyref)

        methodOrProp match {
          case IRTrees.JSMethodDef(flags, nameTree, params, restParam, body) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)

            val closureFuncName = fctx.genInnerFuncName()
            locally {
              implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                Some(clazz.className),
                closureFuncName,
                Some(jsClassCaptures),
                receiverTyp,
                params ::: restParam.toList,
                List(WasmRefType.anyref)
              )
              WasmExpressionBuilder.generateIRBody(body, IRTypes.AnyType)
              fctx.buildAndAddToContext()
            }
            instrs += ctx.refFuncWithDeclaration(closureFuncName)

            instrs += I32_CONST(if (restParam.isDefined) params.size else -1)
            if (isStatic)
              instrs += CALL(WasmFunctionName.installJSStaticMethod)
            else
              instrs += CALL(WasmFunctionName.installJSMethod)

          case IRTrees.JSPropertyDef(flags, nameTree, optGetter, optSetter) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)

            optGetter match {
              case None =>
                instrs += REF_NULL(WasmHeapType.Func)

              case Some(getterBody) =>
                val closureFuncName = fctx.genInnerFuncName()
                locally {
                  implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                    Some(clazz.className),
                    closureFuncName,
                    Some(jsClassCaptures),
                    receiverTyp,
                    Nil,
                    List(WasmRefType.anyref)
                  )
                  WasmExpressionBuilder.generateIRBody(getterBody, IRTypes.AnyType)
                  fctx.buildAndAddToContext()
                }
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            optSetter match {
              case None =>
                instrs += REF_NULL(WasmHeapType.Func)

              case Some((setterParamDef, setterBody)) =>
                val closureFuncName = fctx.genInnerFuncName()
                locally {
                  implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                    Some(clazz.className),
                    closureFuncName,
                    Some(jsClassCaptures),
                    receiverTyp,
                    setterParamDef :: Nil,
                    Nil
                  )
                  WasmExpressionBuilder.generateIRBody(setterBody, IRTypes.NoType)
                  fctx.buildAndAddToContext()
                }
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            if (isStatic)
              instrs += CALL(WasmFunctionName.installJSStaticProperty)
            else
              instrs += CALL(WasmFunctionName.installJSProperty)
        }
      }

      // Static fields
      for (fieldDef <- clazz.fields if fieldDef.flags.namespace.isStatic) {
        // Load class value
        instrs += LOCAL_GET(jsClassLocal)

        // Load name
        fieldDef match {
          case IRTrees.FieldDef(_, name, _, _) =>
            throw new AssertionError(
              s"Unexpected private static field ${name.name.nameString} "
                + s"in JS class ${clazz.className.nameString}"
            )
          case IRTrees.JSFieldDef(_, nameTree, _) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)
        }

        // Generate boxed representation of the zero of the field
        WasmExpressionBuilder.generateIRBody(IRTypes.zeroOf(fieldDef.ftpe), IRTypes.AnyType)

        instrs += CALL(WasmFunctionName.installJSField)
      }

      // Class initializer
      for (classInit <- clazz.methods.find(_.methodName.isClassInitializer)) {
        assert(
          clazz.jsClassCaptures.isEmpty,
          s"Illegal class initializer in non-static class ${clazz.className.nameString}"
        )
        val namespace = IRTrees.MemberNamespace.StaticConstructor
        instrs += CALL(WasmFunctionName(namespace, clazz.className, IRNames.ClassInitializerName))
      }

      // Final result
      instrs += LOCAL_GET(jsClassLocal)

      fctx.buildAndAddToContext()
    }
  }

  private def genLoadJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val cachedJSClassGlobal = WasmGlobal(
      WasmGlobalName.forJSClassValue(clazz.className),
      WasmRefType.anyref,
      WasmExpr(List(REF_NULL(WasmHeapType.Any))),
      isMutable = true
    )
    ctx.addGlobal(cachedJSClassGlobal)

    val fctx = WasmFunctionContext(
      Some(clazz.className),
      WasmFunctionName.loadJSClass(clazz.className),
      None,
      Nil,
      List(WasmRefType.any)
    )

    import fctx.instrs

    fctx.block(WasmRefType.any) { doneLabel =>
      // Load cached JS class, return if non-null
      instrs += GLOBAL_GET(cachedJSClassGlobal.name)
      instrs += BR_ON_NON_NULL(doneLabel)
      // Otherwise, call createJSClass -- it will also store the class in the cache
      instrs += CALL(WasmFunctionName.createJSClassOf(clazz.className))
    }

    fctx.buildAndAddToContext()
  }

  private def genLoadJSModuleFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val className = clazz.className
    val cacheGlobalName = WasmGlobalName.forModuleInstance(className)

    ctx.addGlobal(
      WasmGlobal(
        cacheGlobalName,
        WasmRefType.anyref,
        WasmExpr(List(REF_NULL(WasmHeapType.Any))),
        isMutable = true
      )
    )

    val fctx = WasmFunctionContext(
      WasmFunctionName.loadModule(className),
      Nil,
      List(WasmRefType.anyref)
    )

    import fctx.instrs

    fctx.block(WasmRefType.anyref) { doneLabel =>
      // Load cached instance; return if non-null
      instrs += GLOBAL_GET(cacheGlobalName)
      instrs += BR_ON_NON_NULL(doneLabel)

      // Get the JS class and instantiate it
      instrs += CALL(WasmFunctionName.loadJSClass(className))
      instrs += CALL(WasmFunctionName.jsNewArray)
      instrs += CALL(WasmFunctionName.jsNew)

      // Store and return the result
      instrs += GLOBAL_SET(cacheGlobalName)
      instrs += GLOBAL_GET(cacheGlobalName)
    }

    fctx.buildAndAddToContext()
  }

  private def transformTopLevelMethodExportDef(
      exportDef: IRTrees.TopLevelMethodExportDef
  )(implicit ctx: WasmContext): Unit = {
    val method = exportDef.methodDef
    val exportedName = exportDef.topLevelExportName

    if (method.restParam.isDefined) {
      throw new UnsupportedOperationException(
        s"Top-level export with ...rest param is unsupported at ${method.pos}: $method"
      )
    }

    implicit val fctx = WasmFunctionContext(
      enclosingClassName = None,
      Names.WasmFunctionName.forExport(exportedName),
      receiverTyp = None,
      method.args,
      IRTypes.AnyType
    )

    WasmExpressionBuilder.generateIRBody(method.body, IRTypes.AnyType)

    val func = fctx.buildAndAddToContext()

    ctx.addExport(WasmExport.Function(exportedName, func.name))
  }

  private def transformTopLevelFieldExportDef(
      exportDef: IRTrees.TopLevelFieldExportDef
  )(implicit ctx: WasmContext): Unit = {
    val exprt = WasmExport.Global(
      exportDef.exportName,
      WasmGlobalName.forStaticField(exportDef.field.name)
    )
    ctx.addExport(exprt)
  }

  private def genFunction(
      clazz: LinkedClass,
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext): WasmFunction = {
    val functionName = Names.WasmFunctionName(
      method.flags.namespace,
      clazz.name.name,
      method.name.name
    )

    // Receiver type for non-constructor methods needs to be `(ref any)` because params are invariant
    // Otherwise, vtable can't be a subtype of the supertype's subtype
    // Constructor can use the exact type because it won't be registered to vtables.
    val receiverTyp =
      if (method.flags.namespace.isStatic)
        None
      else if (clazz.kind == ClassKind.HijackedClass)
        Some(transformType(IRTypes.BoxedClassToPrimType(clazz.name.name)))
      else if (method.flags.namespace.isConstructor)
        Some(WasmRefType.nullable(WasmTypeName.WasmStructTypeName.forClass(clazz.name.name)))
      else
        Some(WasmRefType.any)

    // Prepare for function context, set receiver and parameters
    implicit val fctx = WasmFunctionContext(
      Some(clazz.className),
      functionName,
      receiverTyp,
      method.args,
      method.resultType
    )

    // build function body
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    WasmExpressionBuilder.generateIRBody(body, method.resultType)

    fctx.buildAndAddToContext()
  }

  private def transformField(
      field: IRTrees.FieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    WasmStructField(
      Names.WasmFieldName.forClassInstanceField(field.name.name),
      transformType(field.ftpe),
      // needs to be mutable even if it's flags.isMutable = false
      // because it's initialized by constructor
      isMutable = true // field.flags.isMutable
    )
  }
}
