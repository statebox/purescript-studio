'use strict'
import * as path from 'path'

let psRootPath = path.join('/', 'Users', 'erik', 'dev', 'statebox', 'studio', 'stbx-lang', 'output')

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