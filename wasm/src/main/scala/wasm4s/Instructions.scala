package wasm.wasm4s
// https://webassembly.github.io/spec/core/syntax/instructions.html

import Types._
import Names._
import Names.WasmTypeName._

abstract sealed class WasmInstr(
    val mnemonic: String,
    val opcode: Int,
    val immediates: List[WasmImmediate] = Nil
)
object WasmInstr {
  import WasmImmediate._

  sealed trait StackPolymorphicInstr extends WasmInstr

  case object I32_EQZ extends WasmInstr("i32.eqz", 0x45)
  case object I64_EQZ extends WasmInstr("i64.eqz", 0x50)
  case object I32_CLZ extends WasmInstr("i32.clz", 0x67)
  case object I32_CTZ extends WasmInstr("i32.ctz", 0x68)
  case object I32_POPCNT extends WasmInstr("i32.popcnt", 0x69)
  case object I64_CLZ extends WasmInstr("i64.clz", 0x79)
  case object I64_CTZ extends WasmInstr("i64.ctz", 0x7A)
  case object I64_POPCNT extends WasmInstr("i64.popcnt", 0x7B)
  case object F32_ABS extends WasmInstr("f32.abs", 0x8B)
  case object F32_NEG extends WasmInstr("f32.neg", 0x8C)
  case object F32_CEIL extends WasmInstr("f32.ceil", 0x8D)
  case object F32_FLOOR extends WasmInstr("f32.floor", 0x8E)
  case object F32_TRUNC extends WasmInstr("f32.trunc", 0x8F)
  case object F32_NEAREST extends WasmInstr("f32.nearest", 0x90)
  case object F32_SQRT extends WasmInstr("f32.sqrt", 0x91)
  case object F64_ABS extends WasmInstr("f64.abs", 0x99)
  case object F64_NEG extends WasmInstr("f64.neg", 0x9A)
  case object F64_CEIL extends WasmInstr("f64.ceil", 0x9B)
  case object F64_FLOOR extends WasmInstr("f64.floor", 0x9C)
  case object F64_TRUNC extends WasmInstr("f64.trunc", 0x9D)
  case object F64_NEAREST extends WasmInstr("f64.nearest", 0x9E)
  case object F64_SQRT extends WasmInstr("f64.sqrt", 0x9F)
  case object I32_WRAP_I64 extends WasmInstr("i32.wrap_i64", 0xA7)
  case object I32_TRUNC_F32_S extends WasmInstr("i32.trunc_f32_s", 0xA8)
  case object I32_TRUNC_F32_U extends WasmInstr("i32.trunc_f32_u", 0xA9)
  case object I32_TRUNC_F64_S extends WasmInstr("i32.trunc_f64_s", 0xAA)
  case object I32_TRUNC_F64_U extends WasmInstr("i32.trunc_f64_u", 0xAB)
  case object I64_EXTEND_I32_S extends WasmInstr("i64.extend_i32_s", 0xAC)
  case object I64_EXTEND_I32_U extends WasmInstr("i64.extend_i32_u", 0xAD)
  case object I64_TRUNC_F32_S extends WasmInstr("i64.trunc_f32_s", 0xAE)
  case object I64_TRUNC_F32_U extends WasmInstr("i64.trunc_f32_u", 0xAF)
  case object I64_TRUNC_F64_S extends WasmInstr("i64.trunc_f64_s", 0xB0)
  case object I64_TRUNC_F64_U extends WasmInstr("i64.trunc_f64_u", 0xB1)
  case object F32_CONVERT_I32_S extends WasmInstr("f32.convert_i32_s", 0xB2)
  case object F32_CONVERT_I32_U extends WasmInstr("f32.convert_i32_u", 0xB3)
  case object F32_CONVERT_I64_S extends WasmInstr("f32.convert_i64_s", 0xB4)
  case object F32_CONVERT_I64_U extends WasmInstr("f32.convert_i64_u", 0xB5)
  case object F32_DEMOTE_F64 extends WasmInstr("f32.demote_f64", 0xB6)
  case object F64_CONVERT_I32_S extends WasmInstr("f64.convert_i32_s", 0xB7)
  case object F64_CONVERT_I32_U extends WasmInstr("f64.convert_i32_u", 0xB8)
  case object F64_CONVERT_I64_S extends WasmInstr("f64.convert_i64_s", 0xB9)
  case object F64_CONVERT_I64_U extends WasmInstr("f64.convert_i64_u", 0xBA)
  case object F64_PROMOTE_F32 extends WasmInstr("f64.promote_f32", 0xBB)
  case object I32_REINTERPRET_F32 extends WasmInstr("i32.reinterpret_f32", 0xBC)
  case object I64_REINTERPRET_F64 extends WasmInstr("i64.reinterpret_f64", 0xBD)
  case object F32_REINTERPRET_I32 extends WasmInstr("f32.reinterpret_i32", 0xBE)
  case object F64_REINTERPRET_I64 extends WasmInstr("f64.reinterpret_i64", 0xBF)
  case object I32_EXTEND8_S extends WasmInstr("i32.extend8_s", 0xC0)
  case object I32_EXTEND16_S extends WasmInstr("i32.extend16_s", 0xC1)
  case object I64_EXTEND8_S extends WasmInstr("i64.extend8_s", 0xC2)
  case object I64_EXTEND16_S extends WasmInstr("i64.extend16_s", 0xC3)
  case object I64_EXTEND32_S extends WasmInstr("i64.extend32_s", 0xC4)
  case object I32_TRUNC_SAT_F64_S extends WasmInstr("i32.trunc_sat_f64_s", 0xFC02)
  case object I64_TRUNC_SAT_F64_S extends WasmInstr("i64.trunc_sat_f64_s", 0xFC06)

  // Binary operations
  case object I32_EQ extends WasmInstr("i32.eq", 0x46)
  case object I32_NE extends WasmInstr("i32.ne", 0x47)
  case object I32_LT_S extends WasmInstr("i32.lt_s", 0x48)
  case object I32_LT_U extends WasmInstr("i32.lt_u", 0x49)
  case object I32_GT_S extends WasmInstr("i32.gt_s", 0x4A)
  case object I32_GT_U extends WasmInstr("i32.gt_u", 0x4B)
  case object I32_LE_S extends WasmInstr("i32.le_s", 0x4C)
  case object I32_LE_U extends WasmInstr("i32.le_u", 0x4D)
  case object I32_GE_S extends WasmInstr("i32.ge_s", 0x4E)
  case object I32_GE_U extends WasmInstr("i32.ge_u", 0x4F)
  case object I64_EQ extends WasmInstr("i64.eq", 0x51)
  case object I64_NE extends WasmInstr("i64.ne", 0x52)
  case object I64_LT_S extends WasmInstr("i64.lt_s", 0x53)
  case object I64_LT_U extends WasmInstr("i64.lt_u", 0x54)
  case object I64_GT_S extends WasmInstr("i64.gt_s", 0x55)
  case object I64_GT_U extends WasmInstr("i64.gt_u", 0x56)
  case object I64_LE_S extends WasmInstr("i64.le_s", 0x57)
  case object I64_LE_U extends WasmInstr("i64.le_u", 0x58)
  case object I64_GE_S extends WasmInstr("i64.ge_s", 0x59)
  case object I64_GE_U extends WasmInstr("i64.ge_u", 0x5A)
  case object F32_EQ extends WasmInstr("f32.eq", 0x5B)
  case object F32_NE extends WasmInstr("f32.ne", 0x5C)
  case object F32_LT extends WasmInstr("f32.lt", 0x5D)
  case object F32_GT extends WasmInstr("f32.gt", 0x5E)
  case object F32_LE extends WasmInstr("f32.le", 0x5F)
  case object F32_GE extends WasmInstr("f32.ge", 0x60)
  case object F64_EQ extends WasmInstr("f64.eq", 0x61)
  case object F64_NE extends WasmInstr("f64.ne", 0x62)
  case object F64_LT extends WasmInstr("f64.lt", 0x63)
  case object F64_GT extends WasmInstr("f64.gt", 0x64)
  case object F64_LE extends WasmInstr("f64.le", 0x65)
  case object F64_GE extends WasmInstr("f64.ge", 0x66)
  case object I32_ADD extends WasmInstr("i32.add", 0x6A)
  case object I32_SUB extends WasmInstr("i32.sub", 0x6B)
  case object I32_MUL extends WasmInstr("i32.mul", 0x6C)
  case object I32_DIV_S extends WasmInstr("i32.div_s", 0x6D)
  case object I32_DIV_U extends WasmInstr("i32.div_u", 0x6E)
  case object I32_REM_S extends WasmInstr("i32.rem_s", 0x6F)
  case object I32_REM_U extends WasmInstr("i32.rem_u", 0x70)
  case object I32_AND extends WasmInstr("i32.and", 0x71)
  case object I32_OR extends WasmInstr("i32.or", 0x72)
  case object I32_XOR extends WasmInstr("i32.xor", 0x73)
  case object I32_SHL extends WasmInstr("i32.shl", 0x74)
  case object I32_SHR_S extends WasmInstr("i32.shr_s", 0x75)
  case object I32_SHR_U extends WasmInstr("i32.shr_u", 0x76)
  case object I32_ROTL extends WasmInstr("i32.rotl", 0x77)
  case object I32_ROTR extends WasmInstr("i32.rotr", 0x78)
  case object I64_ADD extends WasmInstr("i64.add", 0x7C)
  case object I64_SUB extends WasmInstr("i64.sub", 0x7D)
  case object I64_MUL extends WasmInstr("i64.mul", 0x7E)
  case object I64_DIV_S extends WasmInstr("i64.div_s", 0x7F)
  case object I64_DIV_U extends WasmInstr("i64.div_u", 0x80)
  case object I64_REM_S extends WasmInstr("i64.rem_s", 0x81)
  case object I64_REM_U extends WasmInstr("i64.rem_u", 0x82)
  case object I64_AND extends WasmInstr("i64.and", 0x83)
  case object I64_OR extends WasmInstr("i64.or", 0x84)
  case object I64_XOR extends WasmInstr("i64.xor", 0x85)
  case object I64_SHL extends WasmInstr("i64.shl", 0x86)
  case object I64_SHR_S extends WasmInstr("i64.shr_s", 0x87)
  case object I64_SHR_U extends WasmInstr("i64.shr_u", 0x88)
  case object I64_ROTL extends WasmInstr("i64.rotl", 0x89)
  case object I64_ROTR extends WasmInstr("i64.rotr", 0x8A)
  case object F32_ADD extends WasmInstr("f32.add", 0x92)
  case object F32_SUB extends WasmInstr("f32.sub", 0x93)
  case object F32_MUL extends WasmInstr("f32.mul", 0x94)
  case object F32_DIV extends WasmInstr("f32.div", 0x95)
  case object F32_MIN extends WasmInstr("f32.min", 0x96)
  case object F32_MAX extends WasmInstr("f32.max", 0x97)
  case object F32_COPYSIGN extends WasmInstr("f32.copysign", 0x98)
  case object F64_ADD extends WasmInstr("f64.add", 0xA0)
  case object F64_SUB extends WasmInstr("f64.sub", 0xA1)
  case object F64_MUL extends WasmInstr("f64.mul", 0xA2)
  case object F64_DIV extends WasmInstr("f64.div", 0xA3)
  case object F64_MIN extends WasmInstr("f64.min", 0xA4)
  case object F64_MAX extends WasmInstr("f64.max", 0xA5)

  case class I32_CONST(v: I32) extends WasmInstr("i32.const", 0x41, List(v))
  object I32_CONST { def apply(v: Int): I32_CONST = I32_CONST(I32(v)) }
  case class I64_CONST(v: I64) extends WasmInstr("i64.const", 0x42, List(v))
  object I64_CONST { def apply(v: Long): I64_CONST = I64_CONST(I64(v)) }
  case class F32_CONST(v: F32) extends WasmInstr("f32.const", 0x43, List(v))
  case class F64_CONST(v: F64) extends WasmInstr("f64.const", 0x44, List(v))

  // Memory instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions
  case class I32_LOAD(v: MemArg) extends WasmInstr("i32.load", 0x28, List(v))
  case class I64_LOAD(v: MemArg) extends WasmInstr("i64.load", 0x29, List(v))
  case class F32_LOAD(v: MemArg) extends WasmInstr("f32.load", 0x2A, List(v))
  case class F64_LOAD(v: MemArg) extends WasmInstr("f64.load", 0x2B, List(v))
  case class I32_LOAD8_S(v: MemArg) extends WasmInstr("i32.load8_s", 0x2C, List(v))
  case class I32_LOAD8_U(v: MemArg) extends WasmInstr("i32.load8_u", 0x2D, List(v))
  case class I32_LOAD16_S(v: MemArg) extends WasmInstr("i32.load16_s", 0x2E, List(v))
  case class I32_LOAD16_U(v: MemArg) extends WasmInstr("i32.load16_u", 0x2F, List(v))
  case class I64_LOAD8_S(v: MemArg) extends WasmInstr("i64.load8_s", 0x30, List(v))
  case class I64_LOAD8_U(v: MemArg) extends WasmInstr("i64.load8_u", 0x31, List(v))
  case class I64_LOAD16_S(v: MemArg) extends WasmInstr("i64.load16_s", 0x32, List(v))
  case class I64_LOAD16_U(v: MemArg) extends WasmInstr("i64.load16_u", 0x33, List(v))
  case class I64_LOAD32_S(v: MemArg) extends WasmInstr("i64.load32_s", 0x34, List(v))
  case class I64_LOAD32_U(v: MemArg) extends WasmInstr("i64.load32_u", 0x35, List(v))

  case class I32_STORE(v: MemArg) extends WasmInstr("i32.store", 0x36, List(v))
  case class I64_STORE(v: MemArg) extends WasmInstr("i64.store", 0x37, List(v))
  case class F32_STORE(v: MemArg) extends WasmInstr("f32.store", 0x38, List(v))
  case class F64_STORE(v: MemArg) extends WasmInstr("f64.store", 0x39, List(v))
  case class I32_STORE8(v: MemArg) extends WasmInstr("i32.store8", 0x3A, List(v))
  case class I32_STORE16(v: MemArg) extends WasmInstr("i32.store16", 0x3B, List(v))
  case class I64_STORE8(v: MemArg) extends WasmInstr("i64.store8", 0x3C, List(v))
  case class I64_STORE16(v: MemArg) extends WasmInstr("i64.store16", 0x3D, List(v))
  case class I64_STORE32(v: MemArg) extends WasmInstr("i64.store32", 0x3E, List(v))

  // Control instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
  sealed abstract class StructuredLabeledInstr(
      mnemonic: String,
      opcode: Int,
      immediates: List[WasmImmediate]
  ) extends WasmInstr(mnemonic, opcode, immediates) {
    val label: Option[LabelIdx]
  }

  case object UNREACHABLE extends WasmInstr("unreachable", 0x00) with StackPolymorphicInstr
  case object NOP extends WasmInstr("nop", 0x01)
  case class BLOCK(i: BlockType, label: Option[LabelIdx])
      extends StructuredLabeledInstr("block", 0x02, List(i))
  case class LOOP(i: BlockType, label: Option[LabelIdx])
      extends StructuredLabeledInstr("loop", 0x03, List(i))
  case class IF(i: BlockType, label: Option[LabelIdx] = None)
      extends StructuredLabeledInstr("if", 0x04, List(i))
  case object ELSE extends WasmInstr("else", 0x05)
  case object END extends WasmInstr("end", 0x0B)
  case class BR(i: LabelIdx) extends WasmInstr("br", 0x0C, List(i)) with StackPolymorphicInstr
  case class BR_IF(i: LabelIdx) extends WasmInstr("br_if", 0x0D, List(i))
  case class BR_TABLE(i: LabelIdxVector, default: LabelIdx)
      extends WasmInstr("br_table", 0x0E, List(i, default))
      with StackPolymorphicInstr
  case object RETURN extends WasmInstr("return", 0x0F) with StackPolymorphicInstr
  case class CALL(i: FuncIdx) extends WasmInstr("call", 0x10, List(i))
  case class CALL_INDIRECT(i: TableIdx, t: TypeIdx)
      extends WasmInstr("call_indirect", 0x11, List(i, t))
  case class THROW(i: TagIdx) extends WasmInstr("throw", 0x08, List(i)) with StackPolymorphicInstr
  case object THROW_REF extends WasmInstr("throw_ref", 0x0A) with StackPolymorphicInstr
  case class TRY_TABLE(i: BlockType, cs: CatchClauseVector, label: Option[LabelIdx] = None)
      extends StructuredLabeledInstr("try_table", 0x1F, List(i, cs))

  // Parametric instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
  case object DROP extends WasmInstr("drop", 0x1A)
  // TODO: SELECT

  // Variable instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
  case class LOCAL_GET(i: LocalIdx) extends WasmInstr("local.get", 0x20, List(i))
  case class LOCAL_SET(i: LocalIdx) extends WasmInstr("local.set", 0x21, List(i))
  case class LOCAL_TEE(i: LocalIdx) extends WasmInstr("local.tee", 0x22, List(i))
  case class GLOBAL_GET(i: GlobalIdx) extends WasmInstr("global.get", 0x23, List(i))
  case class GLOBAL_SET(i: GlobalIdx) extends WasmInstr("global.set", 0x24, List(i))

  // Table instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#table-instructions
  // impolement when it's needed

  // Memory instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions

  // ============================================================
  // Reference types
  /** evaluates to the null reference constant.
    *
    * `ref.null rt : [] -> [rtref]` (iff rt = func or rt = extern)
    */
  case class REF_NULL(i: HeapType) extends WasmInstr("ref.null", 0xD0, List(i))

  /** checks for null. ref.is_null : [rtref] -> [i32]
    */
  case object REF_IS_NULL extends WasmInstr("ref.is_null", 0xD1)

  /** creates a reference to a given function. `ref.func $x : [] -> [funcref]` (iff $x : func $t)
    */
  case class REF_FUNC(i: FuncIdx) extends WasmInstr("ref.func", 0xD2, List(i))
  object REF_FUNC {
    def apply(i: WasmFunctionName): REF_FUNC = REF_FUNC(WasmImmediate.FuncIdx(i))
  }

  case object REF_I31 extends WasmInstr("ref.i31", 0xFB1C)
  case object I31_GET_S extends WasmInstr("i31.get_s", 0xFB1D)
  case object I31_GET_U extends WasmInstr("i31.get_u", 0xFB1E)

  // ============================================================
  // Typed Function References
  // https://github.com/WebAssembly/function-references
  case class CALL_REF(i: TypeIdx) extends WasmInstr("call_ref", 0x14, List(i))
  case class RETURN_CALL_REF(i: TypeIdx) extends WasmInstr("return_call_ref", 0x15, List(i))
  case object REF_AS_NOT_NULL extends WasmInstr("ref.as_non_null", 0xD4)
  case class BR_ON_NULL(i: LabelIdx) extends WasmInstr("br_on_null", 0xD5, List(i))
  case class BR_ON_NON_NULL(i: LabelIdx) extends WasmInstr("br_on_non_null", 0xD6, List(i))

  // ============================================================
  // gc
  case class STRUCT_NEW(i: TypeIdx) extends WasmInstr("struct.new", 0xFB00, List(i))
  object STRUCT_NEW {
    def apply(i: WasmTypeName): STRUCT_NEW = STRUCT_NEW(WasmImmediate.TypeIdx(i))
  }
  case class STRUCT_NEW_DEFAULT(i: TypeIdx) extends WasmInstr("struct.new_default", 0xFB01, List(i))
  case class STRUCT_GET(tyidx: TypeIdx, fidx: StructFieldIdx)
      extends WasmInstr("struct.get", 0xFB02, List(tyidx, fidx))
  // STRUCT_GET_S
  // STRUCT_GET_U
  case class STRUCT_SET(tyidx: TypeIdx, fidx: StructFieldIdx)
      extends WasmInstr("struct.set", 0xFB05, List(tyidx, fidx))

  case class ARRAY_NEW(i: TypeIdx) extends WasmInstr("array.new", 0xFB06, List(i))
  case class ARRAY_NEW_DEFAULT(i: TypeIdx) extends WasmInstr("array.new_default", 0xFB07, List(i))
  case class ARRAY_NEW_FIXED(i: TypeIdx, size: I32)
      extends WasmInstr("array.new_fixed", 0xFB08, List(i, size))
  case class ARRAY_NEW_DATA(i: TypeIdx, d: DataIdx)
      extends WasmInstr("array.new_data", 0xFB09, List(i, d))
  case class ARRAY_GET(i: TypeIdx) extends WasmInstr("array.get", 0xFB0B, List(i))
  case class ARRAY_GET_S(i: TypeIdx) extends WasmInstr("array.get_s", 0xFB0C, List(i))
  case class ARRAY_GET_U(i: TypeIdx) extends WasmInstr("array.get_u", 0xFB0D, List(i))
  case class ARRAY_SET(i: TypeIdx) extends WasmInstr("array.set", 0xFB0E, List(i))
  case object ARRAY_LEN extends WasmInstr("array.len", 0xFB0F)
  // ARRAY_FILL,
  // ARRAY_COPY
  // ARRAY_NEW_DATA
  // array_NEW_FIXED

  case object REF_EQ extends WasmInstr("ref.eq", 0xD3)
  case class REF_TEST(i: HeapType) extends WasmInstr("ref.test", 0xFB14, List(i))
  case class REF_TEST_NULL(i: HeapType) extends WasmInstr("ref.test", 0xFB15, List(i))
  case class REF_CAST(i: HeapType) extends WasmInstr("ref.cast", 0xFB16, List(i))
  case class REF_CAST_NULL(i: HeapType) extends WasmInstr("ref.cast", 0xFB17, List(i))

  case class BR_ON_CAST(castFlags: CastFlags, label: LabelIdx, from: HeapType, to: HeapType)
      extends WasmInstr("br_on_cast", 0xFB18, List(castFlags, label, from, to))
  case class BR_ON_CAST_FAIL(castFlags: CastFlags, label: LabelIdx, from: HeapType, to: HeapType)
      extends WasmInstr("br_on_cast_fail", 0xFB19, List(castFlags, label, from, to))
}

abstract sealed trait WasmImmediate
object WasmImmediate {
  case class I32(value: Int) extends WasmImmediate
  case class I64(value: Long) extends WasmImmediate
  case class F32(value: Float) extends WasmImmediate
  case class F64(value: Double) extends WasmImmediate

  // TODO: UInt
  case class MemArg(offset: Long, align: Long) extends WasmImmediate

  /** A structured instruction can consume input and produce output on the operand stack according
    * to its annotated block type. It is given either as a type index that refers to a suitable
    * function type, or as an optional value type inline, which is a shorthand for the function type
    * [] -> [valtype]
    * @see
    *   https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
    */
  abstract sealed trait BlockType extends WasmImmediate
  object BlockType {
    case class FunctionType(ty: WasmFunctionTypeName) extends BlockType
    case class ValueType private (ty: Option[WasmType]) extends BlockType
    object ValueType {
      def apply(ty: WasmType): ValueType = ty match {
        case WasmNoType => ValueType(None)
        case _          => ValueType(Some(ty))
      }
      def apply(): ValueType = ValueType(None)
    }
  }

  case class FuncIdx(val value: WasmFunctionName) extends WasmImmediate
  case class LabelIdx(val value: Int) extends WasmImmediate
  case class LabelIdxVector(val value: List[LabelIdx]) extends WasmImmediate
  case class TypeIdx(val value: WasmTypeName) extends WasmImmediate
  case class DataIdx(val value: WasmDataName) extends WasmImmediate
  case class TableIdx(val value: Int) extends WasmImmediate
  case class TagIdx(val value: WasmTagName) extends WasmImmediate
  case class LocalIdx(val value: WasmLocalName) extends WasmImmediate
  case class GlobalIdx(val value: WasmGlobalName) extends WasmImmediate
  case class HeapType(val value: WasmHeapType) extends WasmImmediate

  case class StructFieldIdx(val value: Int) extends WasmImmediate
  object StructFieldIdx {
    val vtable = StructFieldIdx(0)
    val itables = StructFieldIdx(1)
    val uniqueRegularField = StructFieldIdx(2)
  }

  case class CatchClauseVector(val value: List[CatchClause]) extends WasmImmediate

  /** `castflags` for `br_on_cast` and `br_on_cast_fail`.
    *
    * @see
    *   https://webassembly.github.io/gc/core/binary/instructions.html#control-instructions
    */
  case class CastFlags(nullable1: Boolean, nullable2: Boolean) extends WasmImmediate

  sealed abstract class CatchClause(
      val mnemonic: String,
      val opcode: Int,
      val immediates: List[WasmImmediate]
  )

  object CatchClause {
    case class Catch(x: TagIdx, l: LabelIdx) extends CatchClause("catch", 0x00, List(x, l))
    case class CatchRef(x: TagIdx, l: LabelIdx) extends CatchClause("catch_ref", 0x01, List(x, l))
    case class CatchAll(l: LabelIdx) extends CatchClause("catch_all", 0x02, List(l))
    case class CatchAllRef(l: LabelIdx) extends CatchClause("catch_all_ref", 0x03, List(l))
  }
}
