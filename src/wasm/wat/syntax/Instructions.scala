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

  case RefNull(vaxlue: HeapType)
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
      isSigned: Boolean,
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

  case UnaryOp(dataType: NumType, operation: UnaryOperation)
  case BinaryOp(dataType: NumType, operation: BinaryOperation)
  case ComparisonOp(dataType: NumType, operation: ComparisonOperation)
  case ConversionOp(
      fromType: NumType,
      toType: NumType,
      operation: ConversionOperation
  )

enum UnaryOperation:
  case Clz, Ctz, Popcnt
  case Abs, Neg, Ceil, Floor, Trunc, Nearest, Sqrt
  case Extend8S, Extend16S, Extend32S

enum BinaryOperation:
  case Add, Sub, Mul
  case DivS, DivU, RemS, RemU
  case Div, Min, Max, CopySign

  case And, Or, Xor

  case Shl, ShrS, ShrU
  case Rotl, Rotr

enum ComparisonOperation:
  case Eq, Ne
  case LtS, LtU, GtS, GtU, LeS, LeU, GeS, GeU
  case Lt, Gt, Le, Ge

enum ConversionOperation:
  case ConvertSI32ToF32, ConvertUI32ToF32, ConvertSI64ToF32, ConvertUI64ToF32
  case ConvertSI32ToF64, ConvertUI32ToF64, ConvertSI64ToF64, ConvertUI64ToF64

  case TruncSF32ToI32, TruncUF32ToI32, TruncSF64ToI32, TruncUF64ToI32
  case TruncSF32ToI64, TruncUF32ToI64, TruncSF64ToI64, TruncUF64ToI64

  case DemoteF64ToF32, PromoteF32ToF64

  case ExtendSI32ToI64, ExtendUI32ToI64

  case TruncSatSF32ToI32, TruncSatUF32ToI32
  case TruncSatSF64ToI32, TruncSatUF64ToI32
  case TruncSatSF32ToI64, TruncSatUF32ToI64
  case TruncSatSF64ToI64, TruncSatUF64ToI64

  case ReinterpretI32ToF32, ReinterpretI64ToF64
  case ReinterpretF32ToI32, ReinterpretF64ToI64
