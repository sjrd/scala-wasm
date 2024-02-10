package wasm
package ir2wasm

import wasm4s._
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.ClassKind

import collection.mutable

class WasmBuilder {
  val module = new WasmModule()

  def transformClassDef(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    clazz.kind match {
      case ClassKind.ModuleClass => transformModuleClass(clazz)
      case _                     =>
    }
  }

  def transformModuleClass(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    // type definition
    val fields = clazz.fields.map(transformField)
    val structType = WasmStructType(
      Names.WasmGCTypeName.fromIR(clazz.name.name),
      fields,
      None // TODO
    )
    ctx.gcTypes.define(structType)
    module.addRecGroupType(structType)

    // global instance
    // (global name (ref null type))
    val global = WasmGlobal(
      Names.WasmGlobalName.forModuleClassInstance(clazz.name.name),
      Types.WasmRefNullType(Types.WasmHeapType.Type(structType.name))
    )
    ctx.globals.define(global)
    module.addGlobal(global)

    clazz.topLevelExportDefs.foreach { i =>
      implicit val fctx = WasmFunctionContext()
      val expressionBuilder = new WasmExpressionBuilder(ctx, fctx)
      i match {
        case d: IRTrees.TopLevelFieldExportDef   => ???
        case d: IRTrees.TopLevelJSClassExportDef => ???
        case d: IRTrees.TopLevelMethodExportDef  => transformToplevelMethodExportDef(d)
        case d: IRTrees.TopLevelModuleExportDef  => ???
      }
    }

    val receiver = WasmLocal(
      Names.WasmLocalName.fromStr("<this>"),
      global.typ,
      isParameter = true
    )
    clazz.methods.foreach { method =>
      val paramTys = receiver.typ +: method.args.map(arg => TypeTransformer.transform(arg.ptpe))
      val resultTy = TypeTransformer.transformResultType(method.resultType)
      val funcType = WasmFunctionType(
        Names.WasmFunctionTypeName.fromIR(method.name.name),
        paramTys,
        resultTy
      )
      ctx.functionTypes.define(funcType)
      module.addFunctionType(funcType) // TODO: normalize

      implicit val fctx = WasmFunctionContext(receiver)

      val params = receiver +: method.args.map { arg =>
        WasmLocal(
          Names.WasmLocalName.fromIR(arg.name.name),
          TypeTransformer.transform(arg.ptpe),
          isParameter = true
        )
      }
      params.foreach(fctx.locals.define)

      val body = transformMethod(method)
      val func = WasmFunction(
        Names.WasmFunctionName.fromIR(method.name.name),
        funcType.name,
        params,
        WasmExpr(body)
      )
      ctx.functions.define(func)
      module.addFunction(func)
    }

  }

  private def transformToplevelMethodExportDef(
      exportDef: IRTrees.TopLevelMethodExportDef
  )(implicit ctx: WasmContext, fctx: WasmFunctionContext) = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    val method = exportDef.methodDef
    val methodName = method.name match {
      case lit: IRTrees.StringLiteral => lit
      case _                          => ???
    }

    // hack
    // export top[moduleID="main"] static def "foo"(arg: any): any = {
    //   val prep0: int = arg.asInstanceOf[int];
    //   mod:sample.Main$.foo;I;I(prep0)
    // }
    // ->
    // export top[moduleID="main"] static def "foo"(arg: int): int = {
    //   val prep0: int = arg;
    //   mod:sample.Main$.foo;I;I(arg)
    // }
    val paramTypeMap = mutable.Map[IRTrees.LocalIdent, IRTypes.Type]()
    val nameMap = mutable.Map[IRTrees.LocalIdent, IRTrees.LocalIdent]()
    val resultType: IRTypes.Type = method.body.tpe
    def collectMapping(t: IRTrees.Tree): Unit = {
      t match {
        case IRTrees.Block(stats) => stats.foreach(collectMapping)
        case IRTrees.VarDef(lhs, _, _, _, IRTrees.AsInstanceOf(IRTrees.VarRef(ident), tpe)) =>
          paramTypeMap.update(ident, tpe) // arg -> int
          nameMap.update(lhs, ident) // prep0 -> arg
        case _ =>
      }
    }
    def mutateTree(t: IRTrees.Tree): IRTrees.Tree = {
      t match {
        case b: IRTrees.Block => IRTrees.Block(b.stats.map(mutateTree))(b.pos)
        case vdef @ IRTrees.VarDef(_, _, _, _, IRTrees.AsInstanceOf(vref, tpe)) =>
          vdef.copy(rhs = vref)(vdef.pos)
        case app: IRTrees.Apply =>
          app.copy(args = app.args.map(a => mutateTree(a)))(app.tpe)(app.pos)
        case vref: IRTrees.VarRef =>
          val newName = nameMap.getOrElse(vref.ident, throw new Error("Invalid name"))
          vref.copy(ident = newName)(vref.tpe)(vref.pos)
        case t => t
      }
    }

    collectMapping(method.body)
    val newBody = mutateTree(method.body)
    val newParams = method.args.map { arg =>
      paramTypeMap.get(arg.name) match {
        case None         => arg
        case Some(newTpe) => arg.copy(ptpe = newTpe)(arg.pos)
      }
    }

    val paramTys = newParams.map(arg => TypeTransformer.transform(arg.ptpe))
    val resultTy = TypeTransformer.transformResultType(resultType)
    val funcType = WasmFunctionType(
      Names.WasmFunctionTypeName.fromLiteral(methodName),
      paramTys,
      resultTy
    )
    ctx.functionTypes.define(funcType)
    module.addFunctionType(funcType) // TODO: normalize

    val params = newParams.map { arg =>
      WasmLocal(
        Names.WasmLocalName.fromIR(arg.name.name),
        TypeTransformer.transform(arg.ptpe),
        isParameter = true
      )
    }
    params.foreach(fctx.locals.define)

    val instrs = newBody match {
      case t: IRTrees.Block => t.stats.flatMap(builder.transformTree)
      case _                => builder.transformTree(newBody)
    }
    val func = WasmFunction(
      Names.WasmFunctionName.fromLiteral(methodName),
      funcType.name,
      fctx.locals.all,
      WasmExpr(instrs)
    )
    ctx.functions.define(func)
    module.addFunction(func)

  }

  private def transformMethod(
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext, fctx: WasmFunctionContext): List[WasmInstr] = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    val prefix =
      if (method.flags.namespace.isConstructor) builder.objectCreationPrefix(method) else Nil
    val instrs = body match {
      case t: IRTrees.Block => t.stats.flatMap(builder.transformTree)
      case _                => builder.transformTree(body)
    }
    method.resultType match {
      case IRTypes.NoType => prefix ++ instrs
      case _              => prefix ++ instrs :+ WasmInstr.RETURN
    }
  }

  private def transformField(
      field: IRTrees.AnyFieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    val fieldName =
      field match {
        case f: IRTrees.FieldDef =>
          Names.WasmFieldName.fromIR(f.name.name)
        // TODO
        case js: IRTrees.JSFieldDef => ???
      }
    WasmStructField(
      fieldName,
      TypeTransformer.transform(field.ftpe),
      field.flags.isMutable
    )
  }
}
