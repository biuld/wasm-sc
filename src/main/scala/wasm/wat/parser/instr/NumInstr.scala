package wasm.wat.parser.instr.num

import wasm.wat.parser.f32Lit
import wasm.wat.parser.f64Lit
import wasm.wat.parser.i32Lit
import wasm.wat.parser.i64Lit
import wasm.wat.parser.instr.sign
import wasm.wat.parser.keyword
import wasm.wat.parser.types.numType
import wasm.wat.syntax.instr.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.types.NumType
import wasm.wat.syntax.types.ValType.Num

def intUnaryOp =
  (keyword("clz") `$>` IntUnaryOp.Clz) <|>
    (keyword("ctz") `$>` IntUnaryOp.Ctz) <|>
    (keyword("popcnt") `$>` IntUnaryOp.Popcnt)

def intBinaryOp =
  (keyword("add") `$>` IntBinaryOp.Add) <|>
    (keyword("sub") `$>` IntBinaryOp.Sub) <|>
    (keyword("mul") `$>` IntBinaryOp.Mul) <|>
    (keyword("div_s") `$>` IntBinaryOp.DivS) <|>
    (keyword("div_u") `$>` IntBinaryOp.DivU) <|>
    (keyword("rem_s") `$>` IntBinaryOp.RemS) <|>
    (keyword("rem_u") `$>` IntBinaryOp.RemU) <|>
    (keyword("and") `$>` IntBinaryOp.And) <|>
    (keyword("or") `$>` IntBinaryOp.Or) <|>
    (keyword("xor") `$>` IntBinaryOp.Xor) <|>
    (keyword("shl") `$>` IntBinaryOp.Shl) <|>
    (keyword("shr_s") `$>` IntBinaryOp.ShrS) <|>
    (keyword("shr_u") `$>` IntBinaryOp.ShrU) <|>
    (keyword("rotl") `$>` IntBinaryOp.Rotl) <|>
    (keyword("rotr") `$>` IntBinaryOp.Rotr)

def intComparisonOp =
  (keyword("eq") `$>` IntComparisonOp.Eq) <|>
    (keyword("eqz") `$>` IntComparisonOp.EqZ) <|>
    (keyword("ne") `$>` IntComparisonOp.Ne) <|>
    (keyword("lt_s") `$>` IntComparisonOp.LtS) <|>
    (keyword("lt_u") `$>` IntComparisonOp.LtU) <|>
    (keyword("gt_s") `$>` IntComparisonOp.GtS) <|>
    (keyword("gt_u") `$>` IntComparisonOp.GtU) <|>
    (keyword("le_s") `$>` IntComparisonOp.LeS) <|>
    (keyword("le_u") `$>` IntComparisonOp.LeU) <|>
    (keyword("ge_s") `$>` IntComparisonOp.GeS) <|>
    (keyword("ge_u") `$>` IntComparisonOp.GeU)

def floatUnaryOp =
  (keyword("abs") `$>` FloatUnaryOp.Abs) <|>
    (keyword("neg") `$>` FloatUnaryOp.Neg) <|>
    (keyword("ceil") `$>` FloatUnaryOp.Ceil) <|>
    (keyword("floor") `$>` FloatUnaryOp.Floor) <|>
    (keyword("trunc") `$>` FloatUnaryOp.Trunc) <|>
    (keyword("nearest") `$>` FloatUnaryOp.Nearest) <|>
    (keyword("sqrt") `$>` FloatUnaryOp.Sqrt)

def floatBinaryOp =
  (keyword("add") `$>` FloatBinaryOp.Add) <|>
    (keyword("sub") `$>` FloatBinaryOp.Sub) <|>
    (keyword("mul") `$>` FloatBinaryOp.Mul) <|>
    (keyword("div") `$>` FloatBinaryOp.Div) <|>
    (keyword("min") `$>` FloatBinaryOp.Min) <|>
    (keyword("max") `$>` FloatBinaryOp.Max) <|>
    (keyword("copysign") `$>` FloatBinaryOp.CopySign)

def floatComparisonOp =
  (keyword("eq") `$>` FloatComparisonOp.Eq) <|>
    (keyword("ne") `$>` FloatComparisonOp.Ne) <|>
    (keyword("lt") `$>` FloatComparisonOp.Lt) <|>
    (keyword("gt") `$>` FloatComparisonOp.Gt) <|>
    (keyword("le") `$>` FloatComparisonOp.Le) <|>
    (keyword("ge") `$>` FloatComparisonOp.Ge)

def unaryOp =
  (intUnaryOp.map(UnaryOp.IntOp(_))) <|>
    (floatUnaryOp.map(UnaryOp.FloatOp(_)))

def binaryOp =
  (intBinaryOp.map(BinaryOp.IntOp(_))) <|>
    (floatBinaryOp.map(BinaryOp.FloatOp(_)))

def comparisonOp =
  (intComparisonOp.map(ComparisonOp.IntOp(_))) <|>
    (floatComparisonOp.map(ComparisonOp.FloatOp(_)))

def wrap = keyword("i32.wrap_i64") `$>` Conversion(
  NumType.I64,
  NumType.I32,
  ConversionOp.Wrap,
  None
)

def trunc = for
  to <- (keyword("i32.") `$>` NumType.I32)
    <|> (keyword("i64.") `$>` NumType.I64)
  _ <- keyword("trunc_")
  from <- (keyword("f32_") `$>` NumType.F32)
    <|> (keyword("f64_") `$>` NumType.F64)
  s <- sign
yield Conversion(from, to, ConversionOp.Trunc, Some(s))

def truncSat = for
  to <- (keyword("i32.") `$>` NumType.I32)
    <|> (keyword("i64.") `$>` NumType.I64)
  _ <- keyword("trunc_sat_")
  from <- (keyword("f32_") `$>` NumType.F32)
    <|> (keyword("f64_") `$>` NumType.F64)
  s <- sign
yield Conversion(from, to, ConversionOp.TruncSat, Some(s))

def convert = for
  to <- (keyword("f32.") `$>` NumType.F32)
    <|> (keyword("f64.") `$>` NumType.F64)
  _ <- keyword("convert_")
  from <- (keyword("i32_") `$>` NumType.I32)
    <|> (keyword("i64_") `$>` NumType.I64)
  s <- sign
yield Conversion(from, to, ConversionOp.Convert, Some(s))

def demote = keyword("f32.demote_f64") `$>` Conversion(
  NumType.F64,
  NumType.F32,
  ConversionOp.Demote,
  None
)

def promote = keyword("f64.promote_f32") `$>` Conversion(
  NumType.F32,
  NumType.F64,
  ConversionOp.Promote,
  None
)

def reinterpret = for
  to <- numType
  _ <- keyword(".reinterpret_")
  from <- numType
yield Conversion(from, to, ConversionOp.Reinterpret, None)

def extend = for
  _ <- keyword("i64.extend_i32_")
  s <- sign
yield Conversion(NumType.I32, NumType.I64, ConversionOp.Extend, Some(s))

def const = for
  ty <- numType
  _ <- keyword(".const")
  v <- ty match
    case NumType.I32 => i32Lit.map(Val.I32(_))
    case NumType.I64 => i64Lit.map(Val.I64(_))
    case NumType.F32 => f32Lit.map(Val.F32(_))
    case NumType.F64 => f64Lit.map(Val.F64(_))
yield Const(ty, v)

def unary =
  (for
    ty <- numType
    _ <- keyword(".")
    op <- unaryOp
  yield Unary(ty, op)) <|>
    (keyword("i32.extend8_s") `$>` Unary(
      NumType.I32,
      UnaryOp.IntOp(IntUnaryOp.Extend8S)
    )) <|>
    (keyword("i32.extend16_s") `$>` Unary(
      NumType.I32,
      UnaryOp.IntOp(IntUnaryOp.Extend16S)
    )) <|>
    (keyword("i64.extend8_s") `$>` Unary(
      NumType.I64,
      UnaryOp.IntOp(IntUnaryOp.Extend8S)
    )) <|>
    (keyword("i64.extend16_s") `$>` Unary(
      NumType.I64,
      UnaryOp.IntOp(IntUnaryOp.Extend16S)
    )) <|>
    (keyword("i64.extend32_s") `$>` Unary(
      NumType.I64,
      UnaryOp.IntOp(IntUnaryOp.Extend32S)
    ))

def binary = for
  ty <- numType
  _ <- keyword(".")
  op <- binaryOp
yield Binary(ty, op)

def comparison = for
  ty <- numType
  _ <- keyword(".")
  op <- comparisonOp
yield Comparison(ty, op)

def conversion =
  wrap <|> extend <|> trunc <|> truncSat <|> convert <|> demote <|> promote <|> reinterpret

def numInstr = const <|> binary <|> comparison <|> conversion <|> unary
