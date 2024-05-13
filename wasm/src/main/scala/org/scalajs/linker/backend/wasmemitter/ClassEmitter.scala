package org.scalajs.linker.backend.wasmemitter

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, Position}
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.standard.{CoreSpec, LinkedClass, LinkedTopLevelExport}

import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Names => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import EmbeddedConstants._
import SWasmGen._
import VarGen._
import TypeTransformer._
import WasmContext._

class ClassEmitter(coreSpec: CoreSpec) {
  def transformClassDef(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    val classInfo = ctx.getClassInfo(clazz.className)

    if (!clazz.kind.isClass && classInfo.hasRuntimeTypeInfo) {
      // Gen typeData -- for classes, we do it as part of the vtable generation
      val typeRef = ClassRef(clazz.className)
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val typeDataGlobal =
        genTypeDataGlobal(typeRef, genTypeName.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }

    // Declare static fields
    for {
      field @ FieldDef(flags, name, _, ftpe) <- clazz.fields
      if flags.namespace.isStatic
    } {
      val global = wamod.Global(
        genGlobalName.forStaticField(name.name),
        transformType(ftpe),
        wamod.Expr(List(genZeroOf(ftpe))),
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

  def transformTopLevelExport(
      topLevelExport: LinkedTopLevelExport
  )(implicit ctx: WasmContext): Unit = {
    topLevelExport.tree match {
      case d: TopLevelJSClassExportDef => genDelayedTopLevelExport(d.exportName)
      case d: TopLevelModuleExportDef  => genDelayedTopLevelExport(d.exportName)
      case d: TopLevelMethodExportDef  => transformTopLevelMethodExportDef(d)
      case d: TopLevelFieldExportDef   => transformTopLevelFieldExportDef(d)
    }
  }

  private def genTypeDataFieldValues(
      clazz: LinkedClass,
      reflectiveProxies: List[ConcreteMethodInfo]
  )(implicit
      ctx: WasmContext
  ): List[wa.Instr] = {
    import genFieldName.typeData.{reflectiveProxies => _, _}

    val className = clazz.className
    val classInfo = ctx.getClassInfo(className)

    val kind = className match {
      case ObjectClass         => KindObject
      case BoxedUnitClass      => KindBoxedUnit
      case BoxedBooleanClass   => KindBoxedBoolean
      case BoxedCharacterClass => KindBoxedCharacter
      case BoxedByteClass      => KindBoxedByte
      case BoxedShortClass     => KindBoxedShort
      case BoxedIntegerClass   => KindBoxedInteger
      case BoxedLongClass      => KindBoxedLong
      case BoxedFloatClass     => KindBoxedFloat
      case BoxedDoubleClass    => KindBoxedDouble
      case BoxedStringClass    => KindBoxedString

      case _ =>
        clazz.kind match {
          case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass => KindClass
          case ClassKind.Interface                                               => KindInterface
          case _                                                                 => KindJSType
        }
    }

    val isJSClassInstanceFuncOpt = genIsJSClassInstanceFunction(clazz)

    genTypeDataFieldValues(
      kind,
      classInfo.specialInstanceTypes,
      ClassRef(clazz.className),
      isJSClassInstanceFuncOpt,
      reflectiveProxies
    )
  }

  private def genIsJSClassInstanceFunction(clazz: LinkedClass)(implicit
      ctx: WasmContext
  ): Option[wanme.FunctionName] = {
    import org.scalajs.ir.OriginalName.NoOriginalName

    implicit val noPos: Position = Position.NoPosition

    val hasIsJSClassInstance = clazz.kind match {
      case ClassKind.NativeJSClass => clazz.jsNativeLoadSpec.isDefined
      case ClassKind.JSClass       => clazz.jsClassCaptures.isEmpty
      case _                       => false
    }

    if (hasIsJSClassInstance) {
      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.isJSClassInstance(clazz.className),
        noPos
      )
      val xParam = fb.addParam("x", watpe.RefType.anyref)
      fb.setResultType(watpe.Int32)
      fb.setFunctionType(genTypeName.isJSClassInstanceFuncType)

      val instrs = fb

      if (clazz.kind == ClassKind.JSClass && !clazz.hasInstances) {
        /* We need to constant-fold the instance test, to avoid trying to
         * call $loadJSClass.className, since it will not exist at all.
         */
        instrs += wa.I32_CONST(0) // false
      } else {
        instrs += wa.LOCAL_GET(xParam)
        genLoadJSConstructor(instrs, clazz.className)
        instrs += wa.CALL(genFunctionName.jsBinaryOps(JSBinaryOp.instanceof))
        instrs += wa.CALL(genFunctionName.unbox(BooleanRef))
      }

      val func = fb.buildAndAddToModule()
      Some(func.name)
    } else {
      None
    }
  }

  private def genTypeDataFieldValues(
      kind: Int,
      specialInstanceTypes: Int,
      typeRef: NonArrayTypeRef,
      isJSClassInstanceFuncOpt: Option[wanme.FunctionName],
      reflectiveProxies: List[ConcreteMethodInfo]
  )(implicit
      ctx: WasmContext
  ): List[wa.Instr] = {
    val nameStr = typeRef match {
      case typeRef: PrimRef =>
        typeRef.displayName
      case ClassRef(className) =>
        RuntimeClassNameMapperImpl.map(
          coreSpec.semantics.runtimeClassNameMapper,
          className.nameString
        )
    }

    val nameDataValue: List[wa.Instr] = ctx.getConstantStringDataInstr(nameStr)

    val strictAncestorsValue: List[wa.Instr] = {
      typeRef match {
        case ClassRef(className) =>
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
            wa.GLOBAL_GET(genGlobalName.forVTable(ancestor))
          }
          elems :+ wa.ARRAY_NEW_FIXED(genTypeName.typeDataArray, elems.size)
        case _ =>
          wa.REF_NULL(watpe.HeapType.None) :: Nil
      }
    }

    val cloneFunction = {
      val nullref = wa.REF_NULL(watpe.HeapType.NoFunc)
      typeRef match {
        case ClassRef(className) =>
          val classInfo = ctx.getClassInfo(className)
          // If the class is concrete and implements the `java.lang.Cloneable`,
          // `genCloneFunction` should've generated the clone function
          if (!classInfo.isAbstract && classInfo.ancestors.contains(CloneableClass))
            wa.REF_FUNC(genFunctionName.clone(className))
          else nullref
        case _ => nullref
      }
    }

    val isJSClassInstance = isJSClassInstanceFuncOpt match {
      case None           => wa.REF_NULL(watpe.HeapType.NoFunc)
      case Some(funcName) => wa.REF_FUNC(funcName)
    }

    val reflectiveProxiesInstrs: List[wa.Instr] = {
      reflectiveProxies.flatMap { proxyInfo =>
        val proxyId = ctx.getReflectiveProxyId(proxyInfo.methodName)
        List(
          wa.I32_CONST(proxyId),
          wa.REF_FUNC(proxyInfo.tableEntryName),
          wa.STRUCT_NEW(genTypeName.reflectiveProxy)
        )
      } :+ wa.ARRAY_NEW_FIXED(genTypeName.reflectiveProxies, reflectiveProxies.size)
    }

    nameDataValue :::
      List(
        // kind
        wa.I32_CONST(kind),
        // specialInstanceTypes
        wa.I32_CONST(specialInstanceTypes)
      ) ::: (
        // strictAncestors
        strictAncestorsValue
      ) :::
      List(
        // componentType - always `null` since this method is not used for array types
        wa.REF_NULL(watpe.HeapType(genTypeName.typeData)),
        // name - initially `null`; filled in by the `typeDataName` helper
        wa.REF_NULL(watpe.HeapType.Any),
        // the classOf instance - initially `null`; filled in by the `createClassOf` helper
        wa.REF_NULL(watpe.HeapType(genTypeName.ClassStruct)),
        // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
        wa.REF_NULL(watpe.HeapType(genTypeName.ObjectVTable)),
        // clonefFunction - will be invoked from `clone()` method invokaion on the class
        cloneFunction,
        // isJSClassInstance - invoked from the `isInstance()` helper for JS types
        isJSClassInstance
      ) :::
      // reflective proxies - used to reflective call on the class at runtime.
      // Generated instructions create an array of reflective proxy structs, where each struct
      // contains the ID of the reflective proxy and a reference to the actual method implementation.
      reflectiveProxiesInstrs
  }

  private def genTypeDataGlobal(
      typeRef: NonArrayTypeRef,
      typeDataTypeName: wanme.TypeName,
      typeDataFieldValues: List[wa.Instr],
      vtableElems: List[wa.REF_FUNC]
  )(implicit ctx: WasmContext): wamod.Global = {
    val instrs: List[wa.Instr] =
      typeDataFieldValues ::: vtableElems ::: wa.STRUCT_NEW(typeDataTypeName) :: Nil
    wamod.Global(
      genGlobalName.forVTable(typeRef),
      watpe.RefType(typeDataTypeName),
      wamod.Expr(instrs),
      isMutable = false
    )
  }

  /** @return
    *   Optionally returns the generated struct type for this class. If the given LinkedClass is an
    *   abstract class, returns None
    */
  private def transformClassCommon(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): wamod.StructType = {
    val className = clazz.name.name
    val typeRef = ClassRef(className)
    val classInfo = ctx.getClassInfo(className)

    // generate vtable type, this should be done for both abstract and concrete classes
    val vtableTypeName = genVTableType(classInfo)

    val isAbstractClass = !clazz.hasDirectInstances

    // we should't generate global vtable for abstract class because
    // - Can't generate Global vtable because we can't fill the slot for abstract methods
    // - We won't access vtable for abstract classes since we can't instantiate abstract classes, there's no point generating
    //
    // When we don't generate a vtable, we still generate the typeData

    if (!isAbstractClass) {
      // Generate an actual vtable
      val reflectiveProxies =
        classInfo.resolvedMethodInfos.valuesIterator.filter(_.methodName.isReflectiveProxy).toList
      val typeDataFieldValues = genTypeDataFieldValues(clazz, reflectiveProxies)
      val vtableElems = classInfo.tableEntries.map { methodName =>
        wa.REF_FUNC(classInfo.resolvedMethodInfos(methodName).tableEntryName)
      }
      val globalVTable =
        genTypeDataGlobal(typeRef, vtableTypeName, typeDataFieldValues, vtableElems)
      ctx.addGlobal(globalVTable)
      genGlobalClassItable(clazz)
    } else if (classInfo.hasRuntimeTypeInfo) {
      // Only generate typeData
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val globalTypeData =
        genTypeDataGlobal(typeRef, genTypeName.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(globalTypeData)
    }

    // Declare the struct type for the class
    val vtableField = wamod.StructField(
      genFieldName.objStruct.vtable,
      watpe.RefType(vtableTypeName),
      isMutable = false
    )
    val itablesField = wamod.StructField(
      genFieldName.objStruct.itables,
      watpe.RefType.nullable(genTypeName.itables),
      isMutable = false
    )
    val fields = classInfo.allFieldDefs.map(transformField)
    val structTypeName = genTypeName.forClass(clazz.name.name)
    val superType = clazz.superClass.map(s => genTypeName.forClass(s.name))
    val structType = wamod.StructType(
      vtableField +: itablesField +: fields
    )
    val subType = wamod.SubType(structTypeName, isFinal = false, superType, structType)
    ctx.mainRecType.addSubType(subType)

    // Define the `new` function and possibly the `clone` function, unless the class is abstract
    if (!isAbstractClass) {
      genNewDefaultFunc(clazz)
      if (clazz.ancestors.contains(CloneableClass))
        genCloneFunction(clazz)
    }

    structType
  }

  private def genVTableType(
      classInfo: ClassInfo
  )(implicit ctx: WasmContext): wanme.TypeName = {
    val typeName = genTypeName.forVTable(classInfo.name)
    val vtableFields =
      classInfo.tableEntries.map { methodName =>
        wamod.StructField(
          genFieldName.forMethodTableEntry(methodName),
          watpe.RefType(ctx.tableFunctionType(methodName)),
          isMutable = false
        )
      }
    val superType = classInfo.superClass match {
      case None    => genTypeName.typeData
      case Some(s) => genTypeName.forVTable(s)
    }
    val structType = wamod.StructType(CoreWasmLib.typeDataStructFields ::: vtableFields)
    val subType = wamod.SubType(typeName, isFinal = false, Some(superType), structType)
    ctx.mainRecType.addSubType(subType)
    typeName
  }

  /** Generate type inclusion test for interfaces.
    *
    * The expression `isInstanceOf[<interface>]` will be compiled to a CALL to the function
    * generated by this method.
    *
    * TODO: Efficient type inclusion test. The current implementation generates a sparse array of
    * itables, which, although O(1), may not be optimal for large interfaces. More compressed data
    * structures could potentially improve performance in such cases.
    *
    * See https://github.com/tanishiking/scala-wasm/issues/27#issuecomment-2008252049
    */
  private def genInterfaceInstanceTest(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    assert(clazz.kind == ClassKind.Interface)

    val classInfo = ctx.getClassInfo(clazz.className)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.instanceTest(clazz.name.name),
      pos
    )
    val exprParam = fb.addParam("expr", watpe.RefType.anyref)
    fb.setResultType(watpe.Int32)

    val instrs = fb

    val itables = fb.addLocal("itables", watpe.RefType.nullable(genTypeName.itables))
    val exprNonNullLocal = fb.addLocal("exprNonNull", watpe.RefType.any)

    val itableIdx = ctx.getItableIdx(classInfo)
    instrs.block(watpe.RefType.anyref) { testFail =>
      // if expr is not an instance of Object, return false
      instrs += wa.LOCAL_GET(exprParam)
      instrs += wa.BR_ON_CAST_FAIL(
        testFail,
        watpe.RefType.anyref,
        watpe.RefType(genTypeName.ObjectStruct)
      )

      // get itables and store
      instrs += wa.STRUCT_GET(genTypeName.ObjectStruct, genFieldIdx.objStruct.itables)
      instrs += wa.LOCAL_SET(itables)

      // Dummy return value from the block
      instrs += wa.REF_NULL(watpe.HeapType.Any)

      // if the itables is null (no interfaces are implemented)
      instrs += wa.LOCAL_GET(itables)
      instrs += wa.BR_ON_NULL(testFail)

      instrs += wa.LOCAL_GET(itables)
      instrs += wa.I32_CONST(itableIdx)
      instrs += wa.ARRAY_GET(genTypeName.itables)
      instrs += wa.REF_TEST(watpe.RefType(genTypeName.forITable(clazz.name.name)))
      instrs += wa.RETURN
    } // test fail

    if (classInfo.isAncestorOfHijackedClass) {
      /* It could be a hijacked class instance that implements this interface.
       * Test whether `jsValueType(expr)` is in the `specialInstanceTypes` bitset.
       * In other words, return `((1 << jsValueType(expr)) & specialInstanceTypes) != 0`.
       *
       * For example, if this class is `Comparable`,
       * `specialInstanceTypes == 0b00001111`, since `jl.Boolean`, `jl.String`
       * and `jl.Double` implement `Comparable`, but `jl.Void` does not.
       * If `expr` is a `number`, `jsValueType(expr) == 3`. We then test whether
       * `(1 << 3) & 0b00001111 != 0`, which is true because `(1 << 3) == 0b00001000`.
       * If `expr` is `undefined`, it would be `(1 << 4) == 0b00010000`, which
       * would give `false`.
       */
      val anyRefToVoidSig =
        wamod.FunctionSignature(List(watpe.RefType.anyref), Nil)

      instrs.block(anyRefToVoidSig) { isNullLabel =>
        // exprNonNull := expr; branch to isNullLabel if it is null
        instrs += wa.BR_ON_NULL(isNullLabel)
        instrs += wa.LOCAL_SET(exprNonNullLocal)

        // Load 1 << jsValueType(expr)
        instrs += wa.I32_CONST(1)
        instrs += wa.LOCAL_GET(exprNonNullLocal)
        instrs += wa.CALL(genFunctionName.jsValueType)
        instrs += wa.I32_SHL

        // return (... & specialInstanceTypes) != 0
        instrs += wa.I32_CONST(classInfo.specialInstanceTypes)
        instrs += wa.I32_AND
        instrs += wa.I32_CONST(0)
        instrs += wa.I32_NE
        instrs += wa.RETURN
      }

      instrs += wa.I32_CONST(0) // false
    } else {
      instrs += wa.DROP
      instrs += wa.I32_CONST(0) // false
    }

    fb.buildAndAddToModule()
  }

  private def genNewDefaultFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(className)
    assert(clazz.hasDirectInstances)

    val structName = genTypeName.forClass(className)
    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.newDefault(className),
      pos
    )
    fb.setResultType(watpe.RefType(structName))

    val instrs = fb

    instrs += wa.GLOBAL_GET(genGlobalName.forVTable(className))

    val interfaces = classInfo.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
    if (!interfaces.isEmpty)
      instrs += wa.GLOBAL_GET(genGlobalName.forITable(className))
    else
      instrs += wa.REF_NULL(watpe.HeapType(genTypeName.itables))

    classInfo.allFieldDefs.foreach { f =>
      instrs += genZeroOf(f.ftpe)
    }
    instrs += wa.STRUCT_NEW(structName)

    fb.buildAndAddToModule()
  }

  /** Generate clone function for the given class, if it is concrete and implements the Cloneable
    * interface. The generated clone function will be registered in the typeData of the class (which
    * resides in the vtable of the class), and will be invoked when the `super.clone()` method is
    * called on the class instance.
    */
  private def genCloneFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.className
    val info = ctx.getClassInfo(className)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.clone(className),
      pos
    )
    val fromParam = fb.addParam("from", watpe.RefType(genTypeName.ObjectStruct))
    fb.setResultType(watpe.RefType(genTypeName.ObjectStruct))
    fb.setFunctionType(genTypeName.cloneFunctionType)

    val instrs = fb

    val heapType = watpe.HeapType(genTypeName.forClass(className))

    val from = fb.addLocal("fromTyped", watpe.RefType.nullable(heapType))
    val result = fb.addLocal("result", watpe.RefType.nullable(heapType))

    instrs += wa.LOCAL_GET(fromParam)
    instrs += wa.REF_CAST(watpe.RefType(heapType))
    instrs += wa.LOCAL_SET(from)

    instrs += wa.CALL(genFunctionName.newDefault(className))
    instrs += wa.LOCAL_SET(result)
    info.allFieldDefs.foreach { field =>
      val fieldIdx = info.getFieldIdx(field.name.name)
      instrs += wa.LOCAL_GET(result)
      instrs += wa.LOCAL_GET(from)
      instrs += wa.STRUCT_GET(genTypeName.forClass(className), fieldIdx)
      instrs += wa.STRUCT_SET(genTypeName.forClass(className), fieldIdx)
    }
    instrs += wa.LOCAL_GET(result)
    instrs += wa.REF_AS_NOT_NULL

    fb.buildAndAddToModule()
  }

  private def genLoadModuleFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    assert(clazz.kind == ClassKind.ModuleClass)
    val ctor = clazz.methods
      .find(_.methodName.isConstructor)
      .getOrElse(throw new Error(s"Module class should have a constructor, ${clazz.name}"))
    val typeName = genTypeName.forClass(clazz.name.name)
    val globalInstanceName = genGlobalName.forModuleInstance(clazz.name.name)

    val ctorName = genFunctionName.forMethod(
      ctor.flags.namespace,
      clazz.name.name,
      ctor.name.name
    )

    val resultTyp = watpe.RefType(typeName)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadModule(clazz.className),
      pos
    )
    fb.setResultType(resultTyp)

    val instanceLocal = fb.addLocal("instance", resultTyp)

    val instrs = fb

    instrs.block(resultTyp) { nonNullLabel =>
      // load global, return if not null
      instrs += wa.GLOBAL_GET(globalInstanceName)
      instrs += wa.BR_ON_NON_NULL(nonNullLabel)

      // create an instance and call its constructor
      instrs += wa.CALL(genFunctionName.newDefault(clazz.name.name))
      instrs += wa.LOCAL_TEE(instanceLocal)
      instrs += wa.CALL(ctorName)

      // store it in the global
      instrs += wa.LOCAL_GET(instanceLocal)
      instrs += wa.GLOBAL_SET(globalInstanceName)

      // return it
      instrs += wa.LOCAL_GET(instanceLocal)
    }

    fb.buildAndAddToModule()
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
      val globalName = genGlobalName.forITable(clazz.className)
      ctx.addGlobalITable(clazz.className, genITableGlobal(globalName))
    }
  }

  private def genArrayClassItable()(implicit ctx: WasmContext): Unit =
    ctx.addGlobal(genITableGlobal(genGlobalName.arrayClassITable))

  private def genITableGlobal(
      name: wanme.GlobalName
  )(implicit ctx: WasmContext): wamod.Global = {
    val itablesInit = List(
      wa.I32_CONST(ctx.itablesLength),
      wa.ARRAY_NEW_DEFAULT(genTypeName.itables)
    )
    wamod.Global(
      name,
      watpe.RefType(genTypeName.itables),
      wamod.Expr(itablesInit),
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
    val itableTypeName = genTypeName.forITable(className)
    val itableType = wamod.StructType(
      classInfo.tableEntries.map { methodName =>
        wamod.StructField(
          genFieldName.forMethodTableEntry(methodName),
          watpe.RefType(ctx.tableFunctionType(methodName)),
          isMutable = false
        )
      }
    )
    ctx.mainRecType.addSubType(itableTypeName, itableType)

    if (clazz.hasInstanceTests)
      genInterfaceInstanceTest(clazz)
  }

  private def transformModuleClass(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    transformClassCommon(clazz)

    if (clazz.hasInstances) {
      val heapType = watpe.HeapType(genTypeName.forClass(clazz.className))

      // global instance
      // (global name (ref null type))
      val global = wamod.Global(
        genGlobalName.forModuleInstance(clazz.name.name),
        watpe.RefType.nullable(heapType),
        wamod.Expr(List(wa.REF_NULL(heapType))),
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
        case FieldDef(flags, name, _, _) if !flags.namespace.isStatic =>
          ctx.addGlobal(
            wamod.Global(
              genGlobalName.forJSPrivateField(name.name),
              watpe.RefType.anyref,
              wamod.Expr(List(wa.REF_NULL(watpe.HeapType.Any))),
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

    val preSuperStatsFunctionName = genFunctionName.preSuperStats(clazz.className)
    val superArgsFunctionName = genFunctionName.superArgs(clazz.className)
    val postSuperStatsFunctionName = genFunctionName.postSuperStats(clazz.className)
    val ctor = clazz.jsConstructorDef.get

    FunctionEmitter.emitJSConstructorFunctions(
      preSuperStatsFunctionName,
      superArgsFunctionName,
      postSuperStatsFunctionName,
      clazz.className,
      jsClassCaptures,
      ctor
    )

    // Build the actual `createJSClass` function
    val createJSClassFun = {
      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.createJSClassOf(clazz.className),
        clazz.pos
      )
      val classCaptureParams = jsClassCaptures.map { cc =>
        fb.addParam("cc." + cc.name.name.nameString, transformType(cc.ptpe))
      }
      fb.setResultType(watpe.RefType.any)

      val instrs = fb

      val dataStructTypeName = ctx.getClosureDataStructType(jsClassCaptures.map(_.ptpe))

      // --- Internal name management of `createJSClass`

      val dataStructLocal = fb.addLocal("classCaptures", watpe.RefType(dataStructTypeName))
      val jsClassLocal = fb.addLocal("jsClass", watpe.RefType.any)

      var lastInnerFuncIndex = -1
      def genInnerFuncName(): wanme.FunctionName = {
        lastInnerFuncIndex += 1
        wanme.FunctionName(fb.functionName.name + "__c" + lastInnerFuncIndex)
      }

      // --- Actual start of instructions of `createJSClass`

      // Bundle class captures in a capture data struct -- leave it on the stack for createJSClass
      for (classCaptureParam <- classCaptureParams)
        instrs += wa.LOCAL_GET(classCaptureParam)
      instrs += wa.STRUCT_NEW(dataStructTypeName)
      instrs += wa.LOCAL_TEE(dataStructLocal)

      val classCaptureParamsOfTypeAny: Map[LocalName, wanme.LocalName] = {
        jsClassCaptures
          .zip(classCaptureParams)
          .collect { case (ParamDef(ident, _, AnyType, _), param) =>
            ident.name -> param
          }
          .toMap
      }

      def genLoadIsolatedTree(tree: Tree): Unit = {
        tree match {
          case StringLiteral(value) =>
            // Common shape for all the `nameTree` expressions
            instrs ++= ctx.getConstantStringInstr(value)

          case VarRef(LocalIdent(localName)) if classCaptureParamsOfTypeAny.contains(localName) =>
            /* Common shape for the `jsSuperClass` value
             * We can only deal with class captures of type `AnyType` in this way,
             * since otherwise we might need `adapt` to box the values.
             */
            instrs += wa.LOCAL_GET(classCaptureParamsOfTypeAny(localName))

          case _ =>
            // For everything else, put the tree in its own function and call it
            val closureFuncName = genInnerFuncName()
            FunctionEmitter.emitFunction(
              closureFuncName,
              enclosingClassName = None,
              Some(jsClassCaptures),
              receiverTyp = None,
              paramDefs = Nil,
              restParam = None,
              tree,
              AnyType
            )
            instrs += wa.LOCAL_GET(dataStructLocal)
            instrs += wa.CALL(closureFuncName)
        }
      }

      /* Load super constructor; specified by
       * https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-classdef-runtime-semantics-evaluation
       * - if `jsSuperClass` is defined, evaluate it;
       * - otherwise load the JS constructor of the declared superClass,
       *   as if by `LoadJSConstructor`.
       */
      clazz.jsSuperClass match {
        case None =>
          genLoadJSConstructor(instrs, clazz.superClass.get.name)
        case Some(jsSuperClassTree) =>
          genLoadIsolatedTree(jsSuperClassTree)
      }

      // Load the references to the 3 functions that make up the constructor
      instrs += ctx.refFuncWithDeclaration(preSuperStatsFunctionName)
      instrs += ctx.refFuncWithDeclaration(superArgsFunctionName)
      instrs += ctx.refFuncWithDeclaration(postSuperStatsFunctionName)

      // Load the array of field names and initial values
      instrs += wa.CALL(genFunctionName.jsNewArray)
      for (fieldDef <- clazz.fields if !fieldDef.flags.namespace.isStatic) {
        // Append the name
        fieldDef match {
          case FieldDef(_, name, _, _) =>
            instrs += wa.GLOBAL_GET(genGlobalName.forJSPrivateField(name.name))
          case JSFieldDef(_, nameTree, _) =>
            genLoadIsolatedTree(nameTree)
        }
        instrs += wa.CALL(genFunctionName.jsArrayPush)

        // Append the boxed representation of the zero of the field
        instrs += genBoxedZeroOf(fieldDef.ftpe)
        instrs += wa.CALL(genFunctionName.jsArrayPush)
      }

      // Call the createJSClass helper to bundle everything
      if (ctor.restParam.isDefined) {
        instrs += wa.I32_CONST(ctor.args.size) // number of fixed params
        instrs += wa.CALL(genFunctionName.createJSClassRest)
      } else {
        instrs += wa.CALL(genFunctionName.createJSClass)
      }

      // Store the result, locally in `jsClass` and possibly in the global cache
      if (clazz.jsClassCaptures.isEmpty) {
        // Static JS class with a global cache
        instrs += wa.LOCAL_TEE(jsClassLocal)
        instrs += wa.GLOBAL_SET(genGlobalName.forJSClassValue(clazz.className))
      } else {
        // Local or inner JS class, which is new every time
        instrs += wa.LOCAL_SET(jsClassLocal)
      }

      // Install methods and properties
      for (methodOrProp <- clazz.exportedMembers) {
        val isStatic = methodOrProp.flags.namespace.isStatic
        instrs += wa.LOCAL_GET(dataStructLocal)
        instrs += wa.LOCAL_GET(jsClassLocal)

        val receiverTyp = if (isStatic) None else Some(watpe.RefType.anyref)

        methodOrProp match {
          case JSMethodDef(flags, nameTree, params, restParam, body) =>
            genLoadIsolatedTree(nameTree)

            val closureFuncName = genInnerFuncName()
            FunctionEmitter.emitFunction(
              closureFuncName,
              Some(clazz.className),
              Some(jsClassCaptures),
              receiverTyp,
              params,
              restParam,
              body,
              AnyType
            )
            instrs += ctx.refFuncWithDeclaration(closureFuncName)

            instrs += wa.I32_CONST(if (restParam.isDefined) params.size else -1)
            if (isStatic)
              instrs += wa.CALL(genFunctionName.installJSStaticMethod)
            else
              instrs += wa.CALL(genFunctionName.installJSMethod)

          case JSPropertyDef(flags, nameTree, optGetter, optSetter) =>
            genLoadIsolatedTree(nameTree)

            optGetter match {
              case None =>
                instrs += wa.REF_NULL(watpe.HeapType.Func)

              case Some(getterBody) =>
                val closureFuncName = genInnerFuncName()
                FunctionEmitter.emitFunction(
                  closureFuncName,
                  Some(clazz.className),
                  Some(jsClassCaptures),
                  receiverTyp,
                  paramDefs = Nil,
                  restParam = None,
                  getterBody,
                  resultType = AnyType
                )
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            optSetter match {
              case None =>
                instrs += wa.REF_NULL(watpe.HeapType.Func)

              case Some((setterParamDef, setterBody)) =>
                val closureFuncName = genInnerFuncName()
                FunctionEmitter.emitFunction(
                  closureFuncName,
                  Some(clazz.className),
                  Some(jsClassCaptures),
                  receiverTyp,
                  setterParamDef :: Nil,
                  restParam = None,
                  setterBody,
                  resultType = NoType
                )
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            if (isStatic)
              instrs += wa.CALL(genFunctionName.installJSStaticProperty)
            else
              instrs += wa.CALL(genFunctionName.installJSProperty)
        }
      }

      // Static fields
      for (fieldDef <- clazz.fields if fieldDef.flags.namespace.isStatic) {
        // Load class value
        instrs += wa.LOCAL_GET(jsClassLocal)

        // Load name
        fieldDef match {
          case FieldDef(_, name, _, _) =>
            throw new AssertionError(
              s"Unexpected private static field ${name.name.nameString} "
                + s"in JS class ${clazz.className.nameString}"
            )
          case JSFieldDef(_, nameTree, _) =>
            genLoadIsolatedTree(nameTree)
        }

        // Generate boxed representation of the zero of the field
        instrs += genBoxedZeroOf(fieldDef.ftpe)

        instrs += wa.CALL(genFunctionName.installJSField)
      }

      // Class initializer
      for (classInit <- clazz.methods.find(_.methodName.isClassInitializer)) {
        assert(
          clazz.jsClassCaptures.isEmpty,
          s"Illegal class initializer in non-static class ${clazz.className.nameString}"
        )
        val namespace = MemberNamespace.StaticConstructor
        instrs += wa.CALL(
          genFunctionName.forMethod(namespace, clazz.className, ClassInitializerName)
        )
      }

      // Final result
      instrs += wa.LOCAL_GET(jsClassLocal)

      fb.buildAndAddToModule()
    }
  }

  private def genLoadJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val cachedJSClassGlobal = wamod.Global(
      genGlobalName.forJSClassValue(clazz.className),
      watpe.RefType.anyref,
      wamod.Expr(List(wa.REF_NULL(watpe.HeapType.Any))),
      isMutable = true
    )
    ctx.addGlobal(cachedJSClassGlobal)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadJSClass(clazz.className),
      pos
    )
    fb.setResultType(watpe.RefType.any)

    val instrs = fb

    instrs.block(watpe.RefType.any) { doneLabel =>
      // Load cached JS class, return if non-null
      instrs += wa.GLOBAL_GET(cachedJSClassGlobal.name)
      instrs += wa.BR_ON_NON_NULL(doneLabel)
      // Otherwise, call createJSClass -- it will also store the class in the cache
      instrs += wa.CALL(genFunctionName.createJSClassOf(clazz.className))
    }

    fb.buildAndAddToModule()
  }

  private def genLoadJSModuleFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.className
    val cacheGlobalName = genGlobalName.forModuleInstance(className)

    ctx.addGlobal(
      wamod.Global(
        cacheGlobalName,
        watpe.RefType.anyref,
        wamod.Expr(List(wa.REF_NULL(watpe.HeapType.Any))),
        isMutable = true
      )
    )

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadModule(className),
      pos
    )
    fb.setResultType(watpe.RefType.anyref)

    val instrs = fb

    instrs.block(watpe.RefType.anyref) { doneLabel =>
      // Load cached instance; return if non-null
      instrs += wa.GLOBAL_GET(cacheGlobalName)
      instrs += wa.BR_ON_NON_NULL(doneLabel)

      // Get the JS class and instantiate it
      instrs += wa.CALL(genFunctionName.loadJSClass(className))
      instrs += wa.CALL(genFunctionName.jsNewArray)
      instrs += wa.CALL(genFunctionName.jsNew)

      // Store and return the result
      instrs += wa.GLOBAL_SET(cacheGlobalName)
      instrs += wa.GLOBAL_GET(cacheGlobalName)
    }

    fb.buildAndAddToModule()
  }

  private def transformTopLevelMethodExportDef(
      exportDef: TopLevelMethodExportDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = exportDef.pos

    val method = exportDef.methodDef
    val exportedName = exportDef.topLevelExportName
    val functionName = genFunctionName.forExport(exportedName)

    FunctionEmitter.emitFunction(
      functionName,
      enclosingClassName = None,
      captureParamDefs = None,
      receiverTyp = None,
      method.args,
      method.restParam,
      method.body,
      resultType = AnyType
    )

    /* We cannot directly export the function because it would not be considered
     * a `function`. Instead, we will explicitly create a closure wrapper in the
     * start function and export that instead.
     */
    genDelayedTopLevelExport(exportedName)
  }

  private def transformTopLevelFieldExportDef(
      exportDef: TopLevelFieldExportDef
  )(implicit ctx: WasmContext): Unit = {
    val exprt = wamod.Export.Global(
      exportDef.exportName,
      genGlobalName.forStaticField(exportDef.field.name)
    )
    ctx.addExport(exprt)
  }

  /** Generates a delayed top-level export global, to be initialized in the `start` function.
    *
    * Some top-level exports need to be initialized by run-time code because they need to call
    * initializing functions:
    *
    *   - methods with a `...rest` need to be initialized with the `closureRestNoArg` helper.
    *   - JS classes need to be initialized with their `loadJSClass` helper.
    *   - JS modules need to be initialized with their `loadModule` helper.
    *
    * For all of those, we use `genDelayedTopLevelExport` to generate a Wasm global initialized with
    * `null` and to export it. We actually initialize the global in the `start` function (see
    * `genStartFunction()` in `WasmContext`).
    */
  private def genDelayedTopLevelExport(exportedName: String)(implicit ctx: WasmContext): Unit = {
    val globalName = genGlobalName.forTopLevelExport(exportedName)
    ctx.addGlobal(
      wamod.Global(
        globalName,
        watpe.RefType.anyref,
        wamod.Expr(List(wa.REF_NULL(watpe.HeapType.None))),
        isMutable = true
      )
    )
    ctx.addExport(wamod.Export.Global(exportedName, globalName))
  }

  private def genFunction(
      clazz: LinkedClass,
      method: MethodDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    val className = clazz.className
    val methodName = method.methodName

    val functionName = genFunctionName.forMethod(namespace, className, methodName)

    val isHijackedClass = ctx.getClassInfo(className).kind == ClassKind.HijackedClass

    val receiverTyp =
      if (namespace.isStatic)
        None
      else if (isHijackedClass)
        Some(transformType(BoxedClassToPrimType(className)))
      else
        Some(transformClassType(className).toNonNullable)

    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))

    // Emit the function
    FunctionEmitter.emitFunction(
      functionName,
      Some(className),
      captureParamDefs = None,
      receiverTyp,
      method.args,
      restParam = None,
      body,
      method.resultType
    )

    if (namespace == MemberNamespace.Public && !isHijackedClass) {
      /* Also generate the bridge that is stored in the table entries. In table
       * entries, the receiver type is always `(ref any)`.
       *
       * TODO: generate this only when the method is actually referred to from
       * at least one table.
       */

      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.forTableEntry(className, methodName),
        pos
      )
      val receiverParam = fb.addParam("<this>", watpe.RefType.any)
      val argParams = method.args.map { arg =>
        fb.addParam(arg.name.name.nameString, TypeTransformer.transformType(arg.ptpe))
      }
      fb.setResultTypes(TypeTransformer.transformResultType(method.resultType))
      fb.setFunctionType(ctx.tableFunctionType(methodName))

      val instrs = fb

      // Load and cast down the receiver
      instrs += wa.LOCAL_GET(receiverParam)
      receiverTyp match {
        case Some(watpe.RefType(_, watpe.HeapType.Any)) =>
          () // no cast necessary
        case Some(receiverTyp: watpe.RefType) =>
          instrs += wa.REF_CAST(receiverTyp)
        case _ =>
          throw new AssertionError(s"Unexpected receiver type $receiverTyp")
      }

      // Load the other parameters
      for (argParam <- argParams)
        instrs += wa.LOCAL_GET(argParam)

      // Call the statically resolved method
      instrs += wa.RETURN_CALL(functionName)

      fb.buildAndAddToModule()
    }
  }

  private def transformField(
      field: FieldDef
  )(implicit ctx: WasmContext): wamod.StructField = {
    wamod.StructField(
      genFieldName.forClassInstanceField(field.name.name),
      transformType(field.ftpe),
      // needs to be mutable even if it's flags.isMutable = false
      // because it's initialized by constructor
      isMutable = true // field.flags.isMutable
    )
  }
}
