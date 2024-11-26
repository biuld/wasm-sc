package wasm.wat.parser

import wasm.wat.syntax.Idx
import parser._
import wasm.wat.syntax.Idx.Id

def idx: Parser[Idx] = idLit.map(Idx.Id(_)) <|> i32Lit.map(Idx.U32(_))
