package wasm
package ir2wasm

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}

import wasm4s._
import wasm4s.Names._
import wasm4s.Names.WasmTypeName._
import wasm4s.WasmInstr._
import wasm4s.WasmImmediate._
import org.scalajs.ir.Types.ClassType
import org.scalajs.ir.ClassKind

object WasmExpressionBuilder {
  def transformBody(tree: IRTrees.Tree)(
    implicit ctx: FunctionTypeWriterWasmContext, fctx: WasmFunctionContext
  ): WasmExpr = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    WasmExpr(builder.genBody(tree))
  }
}

private class WasmExpressionBuilder private (ctx: FunctionTypeWriterWasmContext, fctx: WasmFunctionContext) {
  private val instrs = mutable.ListBuffer.empty[WasmInstr]

  def genBody(tree: IRTrees.Tree): List[WasmInstr] = {
    genTree(tree)
    instrs.toList
  }

  /** object creation prefix
    * ```
    * local.get $receiver ;; (ref null $struct)
    * ref.is_null
    * if
    *   call $newDefault
    *   local.set $receiver
    * end
    * ```
    *
    * Maybe we can unreachable in else branch?
    */
  // def objectCreationPrefix(clazz: IRTrees.ClassDef, method: IRTrees.MethodDef): List[WasmInstr] = {
  //   assert(method.flags.namespace.isConstructor)
  //   val recieverGCTypeName = fctx.receiver.typ match {
  //     case Types.WasmRefNullType(Types.WasmHeapType.Type(gcType)) => gcType
  //     case Types.WasmRefType(Types.WasmHeapType.Type(gcType))     => gcType
  //     case _                                                      => ???
  //   }
  //   Nil
  //   // List(
  //   //   LOCAL_GET(LocalIdx(fctx.receiver.name)),
  //   //   REF_IS_NULL,
  //   //   IF(WasmImmediate.BlockType.ValueType(None)),
  //   //   CALL(FuncIdx(WasmFunctionName.newDefault(clazz.name.name))),
  //   //   LOCAL_SET(LocalIdx(fctx.receiver.name)),
  //   //   END
  //   // )
  // }

  def genTrees(trees: List[IRTrees.Tree]): Unit =
    trees.foreach(genTree(_))

  def genTree(tree: IRTrees.Tree): Unit = {
    tree match {
      case t: IRTrees.Literal    => genLiteral(t)
      case t: IRTrees.UnaryOp    => genUnaryOp(t)
      case t: IRTrees.BinaryOp   => genBinaryOp(t)
      case t: IRTrees.VarRef     => genVarRef(t)
      case t: IRTrees.LoadModule => genLoadModule(t)
      case t: IRTrees.StoreModule =>
        genStoreModule(t)
      case t: IRTrees.This => // push receiver to the stack
        instrs += LOCAL_GET(LocalIdx(fctx.receiver.name))
      case t: IRTrees.ApplyStatic => ???
      case t: IRTrees.ApplyStatically =>
        genApplyStatically(t)
      case t: IRTrees.Apply              => genApply(t)
      case t: IRTrees.ApplyDynamicImport => ???
      case t: IRTrees.AsInstanceOf       => genAsInstanceOf(t)
      case t: IRTrees.Block              => genBlock(t)
      case t: IRTrees.Labeled            => genLabeled(t)
      case t: IRTrees.Return             => genReturn(t)
      case t: IRTrees.Select             => genSelect(t)
      case t: IRTrees.Assign             => genAssign(t)
      case t: IRTrees.VarDef             => genVarDef(t)
      case t: IRTrees.New                => genNew(t)
      case t: IRTrees.If                 => genIf(t)
      case t: IRTrees.While              => genWhile(t)
      case t: IRTrees.Skip               => ()
      case _ =>
        println(tree)
        ???

      // case unary: IRTrees.JSUnaryOp => ???
      // case select: IRTrees.JSPrivateSelect => ???
      // case nul: IRTrees.Null => ???
      // case v: IRTrees.UnwrapFromThrowable => ???
      // case IRTrees.RecordValue(pos) =>
      // case IRTrees.JSTypeOfGlobalRef(pos) =>
      // case IRTrees.JSMethodApply(pos) =>
      // case IRTrees.Debugger(pos) =>
      // case IRTrees.JSNewTarget(pos) =>
      // case IRTrees.SelectStatic(tpe) =>
      // case IRTrees.IsInstanceOf(pos) =>
      // case IRTrees.JSLinkingInfo(pos) =>
      // case IRTrees.Select(tpe) =>
      // case IRTrees.Return(pos) =>
      // case IRTrees.ArrayLength(pos) =>
      // case IRTrees.While(pos) =>
      // case IRTrees.LoadJSConstructor(pos) =>
      // case IRTrees.JSSuperMethodCall(pos) =>
      // case IRTrees.NewArray(pos) =>
      // case IRTrees.Match(tpe) =>
      // case IRTrees.Throw(pos) =>
      // case IRTrees.JSNew(pos) =>
      // case IRTrees.Closure(pos) =>
      // case IRTrees.JSGlobalRef(pos) =>
      // case IRTrees.JSBinaryOp(pos) =>
      // case IRTrees.JSObjectConstr(pos) =>
      // case IRTrees.RecordSelect(tpe) =>
      // case IRTrees.AsInstanceOf(pos) =>
      // case IRTrees.TryFinally(pos) =>
      // case IRTrees.Labeled(pos) =>
      // case IRTrees.SelectJSNativeMember(pos) =>
      // case IRTrees.ClassOf(pos) =>
      // case IRTrees.GetClass(pos) =>
      // case IRTrees.JSImportMeta(pos) =>
      // case IRTrees.JSSuperSelect(pos) =>
      // case IRTrees.ArraySelect(tpe) =>
      // case IRTrees.JSSelect(pos) =>
      // case IRTrees.LoadJSModule(pos) =>
      // case IRTrees.JSFunctionApply(pos) =>
      // case IRTrees.WrapAsThrowable(pos) =>
      // case IRTrees.JSSuperConstructorCall(pos) =>
      // case IRTrees.Clone(pos) =>
      // case IRTrees.CreateJSClass(pos) =>
      // case IRTrees.Transient(pos) =>
      // case IRTrees.ArrayValue(pos) =>
      // case IRTrees.JSDelete(pos) =>
      // case IRTrees.ForIn(pos) =>
      // case IRTrees.JSArrayConstr(pos) =>
      // case tc: IRTrees.TryCatch => ???
      // case IRTrees.JSImportCall(pos) =>
      // case IRTrees.IdentityHashCode(pos) =>
    }

  }

  private def genAssign(t: IRTrees.Assign): Unit = {
    t.lhs match {
      case sel: IRTrees.Select =>
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)

        genTree(sel.qualifier)

        // cast if required
        sel.qualifier match {
          case _: IRTrees.This =>
            // requires cast if the qualifier is `this`
            // because receiver type is Object in wasm
            instrs += REF_CAST(
              HeapType(Types.WasmHeapType.Type(WasmStructTypeName(sel.className)))
            )
          case _ =>
            ()
        }

        genTree(t.rhs)
        instrs += STRUCT_SET(TypeIdx(className), idx)

      case sel: IRTrees.SelectStatic => // OK?
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)
        instrs += GLOBAL_GET(
          GlobalIdx(Names.WasmGlobalName.WasmModuleInstanceName.fromIR(sel.className))
        )
        genTree(t.rhs)
        instrs += STRUCT_SET(TypeIdx(className), idx)

      case assign: IRTrees.ArraySelect     => ??? // array.set
      case assign: IRTrees.RecordSelect    => ??? // struct.set
      case assign: IRTrees.JSPrivateSelect => ???
      case assign: IRTrees.JSSelect        => ???
      case assign: IRTrees.JSSuperSelect   => ???
      case assign: IRTrees.JSGlobalRef     => ???

      case ref: IRTrees.VarRef =>
        genTree(t.rhs)
        instrs += LOCAL_SET(LocalIdx(Names.WasmLocalName.fromIR(ref.ident.name)))
    }
  }

  private def genApply(t: IRTrees.Apply): Unit = {
    val receiverClassName = t.receiver.tpe match {
      case ClassType(className)   => className
      case prim: IRTypes.PrimType => IRTypes.PrimTypeToBoxedClass(prim)
      case _                      => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
    }
    val receiverClassInfo = ctx.getClassInfo(receiverClassName)

    if (receiverClassInfo.isInterface) { // interface dispatch
      val itables = ctx.calculateClassItables(clazz = receiverClassName)

      val (itableIdx, methodIdx) = itables.resolveMethod(t.method.name)
      val targetClass = itables.itables(itableIdx)
      val method = targetClass.methods(
        methodIdx
      ) // should be safe since resolveMethod should return valid value

      // val methodIdx = info.methods.indexWhere(i => i.name.methodName == t.method.name.nameString)
      // if (methodIdx < 0) { throw new Error(s"Cannot find method ${t.method}") }
      // val method = info.methods(methodIdx)

      // val rttReceiverClassName =
      //   TypeTransformer.transformType(t.receiver.tpe)(ctx) match {
      //     case Types.WasmRefNullType(Types.WasmHeapType.Type(name @ WasmStructTypeName(_))) => name
      //     case Types.WasmRefType(Types.WasmHeapType.Type(name @ WasmStructTypeName(_)))     => name
      //     case _ => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
      //   }

      genTree(t.receiver)
      genTrees(t.args)
      genTree(t.receiver) // TODO Reuse the receiver computed above
      instrs += STRUCT_GET(
        // receiver type should be upcasted into `Object` if it's interface
        // by TypeTransformer#transformType
        TypeIdx(WasmStructTypeName(IRNames.ObjectClass)),
        StructFieldIdx(1)
      )
      instrs += I32_CONST(I32(itableIdx))
      instrs += ARRAY_GET(
        TypeIdx(WasmArrayType.itables.name)
      )
      instrs += REF_CAST(
        HeapType(Types.WasmHeapType.Type(WasmITableTypeName(targetClass.name)))
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmITableTypeName(targetClass.name)),
        StructFieldIdx(methodIdx)
      )
      instrs += CALL_REF(
        TypeIdx(method.toWasmFunctionType()(ctx).name)
      )
    } else if (receiverClassInfo.kind == ClassKind.HijackedClass) {
      // statically resolved call
      val info = receiverClassInfo.getMethodInfo(t.method.name)
      genTree(t.receiver)
      if (receiverClassName == IRNames.BoxedStringClass && t.receiver.tpe == ClassType(IRNames.BoxedStringClass))
        instrs += REF_CAST(HeapType(Types.WasmHeapType.Type(WasmStructTypeName.string)))
      genTrees(t.args)
      instrs += CALL(FuncIdx(info.name))
    } else { // virtual dispatch
      val (methodIdx, info) = ctx
        .calculateVtable(receiverClassName)
        .resolveWithIdx(WasmFunctionName(receiverClassName, t.method.name))

      // // push args to the stacks
      // local.get $this ;; for accessing funcref
      // local.get $this ;; for accessing vtable
      // struct.get $classType 0 ;; get vtable
      // struct.get $vtableType $methodIdx ;; get funcref
      // call.ref (type $funcType) ;; call funcref
      genTree(t.receiver)
      genTrees(t.args)
      genTree(t.receiver) // TODO Reuse the receiver computed above
      instrs += REF_CAST(
        HeapType(Types.WasmHeapType.Type(WasmStructTypeName(receiverClassName)))
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmStructTypeName(receiverClassName)),
        StructFieldIdx(0)
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmVTableTypeName.fromIR(receiverClassName)),
        StructFieldIdx(methodIdx)
      )
      instrs += CALL_REF(
        TypeIdx(info.toWasmFunctionType()(ctx).name)
      )
    }
  }

  private def genApplyStatically(t: IRTrees.ApplyStatically): Unit = {
    genTree(t.receiver)
    genTrees(t.args)
    val funcName = Names.WasmFunctionName(t.className, t.method.name)
    instrs += CALL(FuncIdx(funcName))
  }

  private def genLiteral(l: IRTrees.Literal): Unit = l match {
    case IRTrees.BooleanLiteral(v) => instrs += WasmInstr.I32_CONST(if (v) I32(1) else I32(0))
    case IRTrees.ByteLiteral(v)    => instrs += WasmInstr.I32_CONST(I32(v))
    case IRTrees.ShortLiteral(v)   => instrs += WasmInstr.I32_CONST(I32(v))
    case IRTrees.IntLiteral(v)     => instrs += WasmInstr.I32_CONST(I32(v))
    case IRTrees.CharLiteral(v)    => instrs += WasmInstr.I32_CONST(I32(v))
    case IRTrees.LongLiteral(v)    => instrs += WasmInstr.I64_CONST(I64(v))
    case IRTrees.FloatLiteral(v)   => instrs += WasmInstr.F32_CONST(F32(v))
    case IRTrees.DoubleLiteral(v)  => instrs += WasmInstr.F64_CONST(F64(v))

    case v: IRTrees.Undefined =>
      instrs += WasmInstr.GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmUndefName))
    case v: IRTrees.Null => ???

    case v: IRTrees.StringLiteral =>
      // TODO We should allocate literal strings once and for all as globals
      val str = v.value
      str.foreach(c => instrs += WasmInstr.I32_CONST(I32(c.toInt)))
      instrs += WasmInstr.ARRAY_NEW_FIXED(TypeIdx(WasmTypeName.WasmArrayTypeName.stringData), I32(str.length()))
      instrs += WasmInstr.STRUCT_NEW(TypeIdx(WasmTypeName.WasmStructTypeName.string))

    case v: IRTrees.ClassOf => ???
  }

  private def genSelect(sel: IRTrees.Select): Unit = {
    val className = WasmStructTypeName(sel.className)
    val fieldName = WasmFieldName(sel.field.name)
    val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)

    genTree(sel.qualifier)

    // cast if required
    sel.qualifier match {
      case _: IRTrees.This =>
        // requires cast if the qualifier is `this`
        // because receiver type is Object in wasm
        instrs += REF_CAST(
          HeapType(Types.WasmHeapType.Type(WasmStructTypeName(sel.className)))
        )
      case _ =>
        ()
    }

    instrs += STRUCT_GET(TypeIdx(className), idx)
  }

  private def genStoreModule(t: IRTrees.StoreModule): Unit = {
    val name = WasmGlobalName.WasmModuleInstanceName.fromIR(t.className)
    genTree(t.value)
    instrs += GLOBAL_SET(GlobalIdx(name))
  }

  /** Push module class instance to the stack.
    *
    * see: WasmBuilder.genLoadModuleFunc
    */
  private def genLoadModule(t: IRTrees.LoadModule): Unit =
    instrs += CALL(FuncIdx(Names.WasmFunctionName.loadModule(t.className)))

  private def genUnaryOp(unary: IRTrees.UnaryOp): Unit = {
    import IRTrees.UnaryOp._

    genTree(unary.lhs)

    (unary.op: @switch) match {
      case Boolean_! =>
        instrs += I32_CONST(I32(1))
        instrs += I32_XOR

      // Widening conversions
      case CharToInt | ByteToInt | ShortToInt =>
        () // these are no-ops because they are all represented as i32's with the right mathematical value
      case IntToLong =>
        instrs += I64_EXTEND32_S
      case IntToDouble =>
        instrs += F64_CONVERT_I32_S
      case FloatToDouble =>
        instrs += F64_PROMOTE_F32

      // Narrowing conversions
      case IntToChar =>
        instrs += I32_CONST(I32(0xffff))
        instrs += I32_AND
      case IntToByte =>
        instrs += I32_EXTEND8_S
      case IntToShort =>
        instrs += I32_EXTEND16_S
      case LongToInt =>
        instrs += I32_WRAP_I64
      case DoubleToInt =>
        instrs += I32_TRUNC_SAT_F64_S
      case DoubleToFloat =>
        instrs += F32_DEMOTE_F64

      // Long <-> Double (neither widening nor narrowing)
      case LongToDouble =>
        instrs += F64_CONVERT_I64_S
      case DoubleToLong =>
        instrs += I64_TRUNC_SAT_F64_S

      // Long -> Float (neither widening nor narrowing), introduced in 1.6
      case LongToFloat =>
        instrs += F32_CONVERT_I64_S

      // String.length, introduced in 1.11
      case String_length =>
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.string), StructFieldIdx(0)) // get the array
        instrs += ARRAY_LEN
    }
  }

  private def genBinaryOp(binary: IRTrees.BinaryOp): Unit = {
    import IRTrees.BinaryOp

    def genLongShiftOp(shiftInstr: WasmInstr): Unit = {
      genTree(binary.lhs)
      genTree(binary.rhs)
      instrs += I64_EXTEND_I32_S
      instrs += shiftInstr
    }

    binary.op match {
      case BinaryOp.String_+ => genStringConcat(binary.lhs, binary.rhs)

      case BinaryOp.Long_<<  => genLongShiftOp(I64_SHL)
      case BinaryOp.Long_>>> => genLongShiftOp(I64_SHR_U)
      case BinaryOp.Long_>>  => genLongShiftOp(I64_SHR_S)

      // New in 1.11
      case BinaryOp.String_charAt =>
        genTree(binary.lhs) // push the string
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.string), StructFieldIdx(0)) // get the array
        genTree(binary.rhs) // push the index
        instrs += ARRAY_GET_U(TypeIdx(WasmArrayTypeName.stringData)) // access the element of the array

      case _ => genElementaryBinaryOp(binary)
    }
  }

  private def genElementaryBinaryOp(binary: IRTrees.BinaryOp): Unit = {
    import IRTrees.BinaryOp
    genTree(binary.lhs)
    genTree(binary.rhs)
    val operation = binary.op match {
      case BinaryOp.=== => ???
      case BinaryOp.!== => ???

      case BinaryOp.Boolean_== => I32_EQ
      case BinaryOp.Boolean_!= => I32_NE
      case BinaryOp.Boolean_|  => I32_OR
      case BinaryOp.Boolean_&  => I32_AND

      case BinaryOp.Int_+   => I32_ADD
      case BinaryOp.Int_-   => I32_SUB
      case BinaryOp.Int_*   => I32_MUL
      case BinaryOp.Int_/   => I32_DIV_S // signed division
      case BinaryOp.Int_%   => I32_REM_S // signed remainder
      case BinaryOp.Int_|   => I32_OR
      case BinaryOp.Int_&   => I32_AND
      case BinaryOp.Int_^   => I32_XOR
      case BinaryOp.Int_<<  => I32_SHL
      case BinaryOp.Int_>>> => I32_SHR_U
      case BinaryOp.Int_>>  => I32_SHR_S
      case BinaryOp.Int_==  => I32_EQ
      case BinaryOp.Int_!=  => I32_NE
      case BinaryOp.Int_<   => I32_LT_S
      case BinaryOp.Int_<=  => I32_LE_S
      case BinaryOp.Int_>   => I32_GT_S
      case BinaryOp.Int_>=  => I32_GE_S

      case BinaryOp.Long_+   => I64_ADD
      case BinaryOp.Long_-   => I64_SUB
      case BinaryOp.Long_*   => I64_MUL
      case BinaryOp.Long_/   => I64_DIV_S
      case BinaryOp.Long_%   => I64_REM_S
      case BinaryOp.Long_|   => I64_OR
      case BinaryOp.Long_&   => I64_AND
      case BinaryOp.Long_^   => I64_XOR

      case BinaryOp.Long_== => I64_EQ
      case BinaryOp.Long_!= => I64_NE
      case BinaryOp.Long_<  => I64_LT_S
      case BinaryOp.Long_<= => I64_LE_S
      case BinaryOp.Long_>  => I64_GT_S
      case BinaryOp.Long_>= => I64_GE_S

      case BinaryOp.Float_+ => F32_ADD
      case BinaryOp.Float_- => F32_SUB
      case BinaryOp.Float_* => F32_MUL
      case BinaryOp.Float_/ => F32_DIV

      // TODO
      // get_local 0
      // get_local 1
      // f32.div
      // f32.trunc
      // get_local 1
      // f32.mul
      // f32.sub
      case BinaryOp.Float_% => ???

      case BinaryOp.Double_+ => F64_ADD
      case BinaryOp.Double_- => F64_SUB
      case BinaryOp.Double_* => F64_MUL
      case BinaryOp.Double_/ => F64_DIV
      case BinaryOp.Double_% => ??? // TODO same as Float_%

      case BinaryOp.Double_== => F64_EQ
      case BinaryOp.Double_!= => F64_NE
      case BinaryOp.Double_<  => F64_LT
      case BinaryOp.Double_<= => F64_LE
      case BinaryOp.Double_>  => F64_GT
      case BinaryOp.Double_>= => F64_GE
    }
    instrs += operation
  }

  private def genStringConcat(lhs: IRTrees.Tree, rhs: IRTrees.Tree): Unit = {
    val wasmStringType = Types.WasmRefType(Types.WasmHeapType.Type(WasmStructTypeName.string))

    def genToString(tree: IRTrees.Tree): Unit = {
      genTree(tree)
      tree.tpe match {
        case IRTypes.StringType =>
          () // no-op

        case IRTypes.BooleanType =>
          instrs += IF(BlockType.ValueType(wasmStringType))
          genLiteral(IRTrees.StringLiteral("true")(tree.pos))
          instrs += ELSE
          genLiteral(IRTrees.StringLiteral("false")(tree.pos))
          instrs += END

        case IRTypes.CharType =>
          instrs += WasmInstr.ARRAY_NEW_FIXED(TypeIdx(WasmTypeName.WasmArrayTypeName.stringData), I32(1))
          instrs += WasmInstr.STRUCT_NEW(TypeIdx(WasmTypeName.WasmStructTypeName.string))

        case IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType =>
          // TODO Write a correct implementation
          instrs += DROP
          genLiteral(IRTrees.StringLiteral("0")(tree.pos))

        case IRTypes.LongType =>
          // TODO Write a correct implementation
          instrs += DROP
          genLiteral(IRTrees.StringLiteral("0")(tree.pos))

        case IRTypes.FloatType | IRTypes.DoubleType =>
          // TODO Write a correct implementation
          instrs += DROP
          genLiteral(IRTrees.StringLiteral("0.0")(tree.pos))

        case _ =>
          // TODO
          ???
      }
    }

    lhs match {
      case IRTrees.StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        genToString(rhs)

      case _ =>
        // TODO: genToString(lhs) ::: genToString(rhs) :: callHelperConcat() :: Nil
        ???
    }
  }

  private def genAsInstanceOf(tree: IRTrees.AsInstanceOf): Unit = {
    genTree(tree.expr)

    val sourceTpe = tree.expr.tpe
    val targetTpe = tree.tpe

    if (IRTypes.isSubtype(sourceTpe, targetTpe)(isSubclass(_, _))) {
      // Common case where no cast is necessary
      ()
    } else {
      println(tree)
      ???
    }
  }

  private def isSubclass(subClass: IRNames.ClassName, superClass: IRNames.ClassName): Boolean =
    ctx.getClassInfo(subClass).ancestors.contains(superClass)

  private def genVarRef(r: IRTrees.VarRef): Unit = {
    val name = WasmLocalName.fromIR(r.ident.name)
    instrs += LOCAL_GET(LocalIdx(name))
  }

  private def genVarDef(r: IRTrees.VarDef): Unit = {
    r.vtpe match {
      // val _: Unit = rhs
      case ClassType(className) if className == IRNames.BoxedUnitClass =>
        genTree(r.rhs)
        instrs += DROP

      case _ =>
        val local = WasmLocal(
          WasmLocalName.fromIR(r.name.name),
          TypeTransformer.transformType(r.vtpe)(ctx),
          isParameter = false
        )
        fctx.locals.define(local)

        genTree(r.rhs)
        instrs += LOCAL_SET(LocalIdx(local.name))
    }
  }

  private def genIf(t: IRTrees.If): Unit = {
    val ty = TypeTransformer.transformType(t.tpe)(ctx)
    genTree(t.cond)
    instrs += IF(BlockType.ValueType(ty))
    genTree(t.thenp)
    instrs += ELSE
    genTree(t.elsep)
    instrs += END
  }

  private def genWhile(t: IRTrees.While): Unit = {
    val label = fctx.genLabel()
    val noResultType = BlockType.ValueType(Types.WasmNoType)

    t.cond match {
      case IRTrees.BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        // loop $label
        //   body
        //   br $label
        // end
        // unreachable
        instrs += LOOP(noResultType, Some(label))
        genTree(t.body)
        instrs += BR(label)
        instrs += END
        instrs += UNREACHABLE

      case _ =>
        // loop $label
        //   cond
        //   if
        //     body
        //     br $label
        //   end
        // end

        instrs += LOOP(noResultType, Some(label))
        genTree(t.cond)
        instrs += IF(noResultType)
        genTree(t.body)
        instrs += BR(label)
        instrs += END // IF
        instrs += END // LOOP
    }
  }

  private def genBlock(t: IRTrees.Block): Unit =
    t.stats.foreach(genTree)

  private def genLabeled(t: IRTrees.Labeled): Unit = {
    val label = fctx.getLabelFor(t.label.name)
    val ty = TypeTransformer.transformType(t.tpe)(ctx)

    instrs += BLOCK(BlockType.ValueType(ty), Some(label))
    genTree(t.body)
    instrs += END
  }

  private def genReturn(t: IRTrees.Return): Unit = {
    val label = fctx.getLabelFor(t.label.name)

    genTree(t.expr)
    instrs += BR(label)
  }

  private def genNew(n: IRTrees.New): Unit = {
    val localInstance = WasmLocal(
      fctx.genSyntheticLocalName(),
      TypeTransformer.transformType(n.tpe)(ctx),
      isParameter = false
    )
    fctx.locals.define(localInstance)

    // REF_NULL(HeapType(Types.WasmHeapType.Type(WasmTypeName.WasmStructTypeName(n.className)))),
    // LOCAL_TEE(LocalIdx(localInstance.name))
    instrs += CALL(FuncIdx(WasmFunctionName.newDefault(n.className)))
    instrs += LOCAL_TEE(LocalIdx(localInstance.name))
    genTrees(n.args)
    instrs += CALL(FuncIdx(WasmFunctionName(n.className, n.ctor.name)))
    instrs += LOCAL_GET(LocalIdx(localInstance.name))
  }
}
