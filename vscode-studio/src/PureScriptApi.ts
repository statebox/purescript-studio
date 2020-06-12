'use strict'
import * as path from 'path'

// This works if we make sure that the '/output' dir containing the compiled PS is copied to '/out/output', where the vscode bundler can find it.
let psRootPath = path.join(__dirname, 'output')

export const PS = {
  Prelude:       require(path.join(psRootPath, 'Prelude', 'index.js')),
  Parser:        require(path.join(psRootPath, 'Language.Statebox', 'index.js')),
  Generator:     require(path.join(psRootPath, 'Language.Statebox.Net.Generator', 'index.js')),
  Generator_Net: require(path.join(psRootPath, 'Language.Statebox.Net.Generator.Net', 'index.js')),
  Either:        require(path.join(psRootPath, 'Data.Either', 'index.js'))
}

// some type aliases to represent the general idea of their actual PureScript counterparts
export type AST = any
export type ParseResult = any
export type PSPosition = { line: number, column: number }
export type Span = { start: PSPosition, end: PSPosition }
export type TextWithSpan = { text: string, span: Span }

export type NetModel = any