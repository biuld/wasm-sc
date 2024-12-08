package wasm.wat.syntax

enum Idx:
  case U32(value: Int)
  case Id(value: String)

type TableIdx = Idx
type ElemIdx = Idx
type LocalIdx = Idx
type GlobalIdx = Idx
type DataIdx = Idx

enum Instruction:
  case Block(
      label: Option[String],
      ty: Option[ValType],
      body: List[Instruction]
  )
  case Loop(
      label: Option[String],
      ty: Option[ValType],
      body: List[Instruction]
  )
  case If(
      label: Option[String],
      ty: Option[ValType],
      thenBr: List[Instruction],
      elseBr: List[Instruction]
  )

  case Unreachable
  case Nop
  case Br(l: Idx)
  case BrIf(l: Idx)
  case BrTable(l: List[Idx])
  case Return
  case Call(x: Idx)
  case CallIndirect(x: Idx, y: Idx)

  case RefNull(value: HeapType)
  case RefIsNull
  case RefFunc(x: Idx)

  case Drop
  case Select(t: Option[ValType])

  case LocalGet(x: LocalIdx)
  case LocalSet(x: LocalIdx)
  case LocalTee(x: LocalIdx)
  case GlobalGet(x: GlobalIdx)
  case GlobalSet(x: GlobalIdx)

  case TableGet(x: TableIdx)
  case TableSet(x: TableIdx)
  case TableSize(x: TableIdx)
  case TableGrow(x: TableIdx)
  case TableFill(x: TableIdx)
  case TableCopy(x: TableIdx, y: TableIdx)
  case TableInit(x: TableIdx, y: ElemIdx)
  case ElemDrop(x: ElemIdx)

  case Load(
      dataType: NumType,
      sign: Option[Boolean],
      subWidth: Option[Int],
      offset: Int,
      align: Int
  )
  case Store(
      dataType: NumType,
      subWidth: Option[Int],
      offset: Int,
      align: Int
  )
  case MemorySize
  case MemoryGrow
  case MemoryFill
  case MemoryCopy
  case MemoryInit(x: DataIdx)
  case DataDrop(x: DataIdx)

  case Const(ty: NumType, v: Val)
  case Unary(ty: NumType, op: UnaryOp)
  case Binary(ty: NumType, op: BinaryOp)
  case Comparison(ty: NumType, op: ComparisonOp)
  case Conversion(
      from: NumType,
      to: NumType,
      op: ConversionOp,
      sign: Option[Boolean]
  )

enum Val:
  case I32(value: Int)
  case I64(value: Long)
  case F32(value: Float)
  case F64(value: Double)

enum IntUnaryOp:
  case Clz, Ctz, Popcnt
  case Extend8S, Extend16S, Extend32S

enum IntBinaryOp:
  case Add, Sub, Mul
  case DivS, DivU, RemS, RemU
  case And, Or, Xor
  case Shl, ShrS, ShrU, Rotl, Rotr

enum IntComparisonOp:
  case Eq, EqZ, Ne
  case LtS, LtU, GtS, GtU, LeS, LeU, GeS, GeU

enum FloatUnaryOp:
  case Abs, Neg, Ceil, Floor, Trunc, Nearest, Sqrt

enum FloatBinaryOp:
  case Add, Sub, Mul, Div
  case Min, Max, CopySign

enum FloatComparisonOp:
  case Eq, Ne, Lt, Gt, Le, Ge

enum UnaryOp:
  case IntOp(op: IntUnaryOp)
  case FloatOp(op: FloatUnaryOp)

enum BinaryOp:
  case IntOp(op: IntBinaryOp)
  case FloatOp(op: FloatBinaryOp)

enum ComparisonOp:
  case IntOp(op: IntComparisonOp)
  case FloatOp(op: FloatComparisonOp)

enum ConversionOp:
  case Wrap
  case Extend
  case Trunc
  case TruncSat
  case Convert
  case Demote
  case Promote
  case Reinterpret
