package wasm.wat.syntax

import wasm.wat.syntax.{ValType, Val}
import wasm.wat.syntax.Val._

/** Represents WebAssembly instructions as a unified ADT */
enum Instr:
  // Control instructions
  case Nop                          // No operation
  case Block(label: Option[Name])   // A labeled block
  case Loop(label: Option[Name])    // A labeled loop
  case If                           // Conditional branch
  case Else                         // Else branch
  case End                          // End block or control structure
  case Br(label: Name)              // Branch to a label
  case BrIf(label: Name)            // Conditional branch to a label
  case Call(index: Id)              // Call a function by index or name
  case Return                       // Return from function

  // Parametric instructions
  case Drop                         // Drop the top value on the stack
  case Select                       // Select between two values based on a condition

  // Variable instructions
  case LocalGet(index: Id)          // Get a local variable
  case LocalSet(index: Id)          // Set a local variable
  case GlobalGet(index: Id)         // Get a global variable
  case GlobalSet(index: Id)         // Set a global variable

  // Memory instructions
  case Load(memType: ValType, offset: Int = 0, align: Int = 0)  // Load value from memory
  case Store(memType: ValType, offset: Int = 0, align: Int = 0) // Store value to memory

  // Numeric instructions
  case Const(value: Val)            // Push constant to the stack
  case Add(valType: ValType)        // Add two numbers
  case Sub(valType: ValType)        // Subtract two numbers
  case Mul(valType: ValType)        // Multiply two numbers
  case Div(valType: ValType)        // Divide two numbers
  case Eq(valType: ValType)         // Equality comparison
  case Ne(valType: ValType)         // Not equal comparison
  case Lt(valType: ValType)         // Less than
  case Gt(valType: ValType)         // Greater than