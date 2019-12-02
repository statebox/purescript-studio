'use strict'
import * as vscode from 'vscode'
import {DocumentSymbol, DocumentSymbolProvider, Range, Position, TextDocument, CancellationToken} from 'vscode'
import {PS, AST, TextWithSpan} from './PureScriptApi'
import * as path from 'path'

export class NetDocumentSymbolProvider implements DocumentSymbolProvider {
    public provideDocumentSymbols(doc: TextDocument, tok: CancellationToken): Thenable<DocumentSymbol[]> {
        let src = doc.getText()
        let astE = PS.Parser.parseNet(src)
        console.log('astE = ' + JSON.stringify(astE, null, 2))

        return PS.Either.either((e: any) => {
            vscode.window.showWarningMessage("Could not show outline because of a problem compiling the net.")
            return Promise.reject("Failed to parse net: " + e)
        })((ast: AST) => {
            let parseResult = PS.Generator.mkParseResult(ast)
            console.log('parseResult = ' + JSON.stringify(astE, null, 2))

            // TODO only use the filename as the net name when we it's not specified in the source
            let fileBaseName = path.basename(doc.uri.fsPath).split(".")[0]
            let netName = fileBaseName

            let symbolModel = PS.Generator.mkTransitionSymbols(parseResult)
            let transitionSymbols: TextWithSpan[] = symbolModel
            let transitionDocumentSymbols = transitionSymbols.map((s) => new DocumentSymbol(
                s.text,
                "()",
                vscode.SymbolKind.Method,
                new Range(new Position(s.span.start.line-1, s.span.start.column-1), new Position(s.span.end.line-1, s.span.end.column-1)),
                new Range(new Position(s.span.start.line-1, s.span.start.column-1), new Position(s.span.end.line-1, s.span.end.column-1)),
            ))

            return Promise.resolve([
                new DocumentSymbolWithChildren(
                    netName,
                    parseResult.description,
                    vscode.SymbolKind.Class,
                    // TODO fix ranges
                    new Range(new Position(0,0), new Position(0, 0)),
                    new Range(new Position(0,0), new Position(0, 0)),
                   transitionDocumentSymbols
                )
            ])
        })(astE)
    }
}

// TODO move to its own file
export class DocumentSymbolWithChildren extends DocumentSymbol {
    /**
     * Creates a new document symbol.
     *
     * @param name The name of the symbol.
     * @param detail Details for the symbol.
     * @param kind The kind of the symbol.
     * @param range The full range of the symbol.
     * @param selectionRange The range that should be reveal.
     */
    constructor(name: string, detail: string, kind: vscode.SymbolKind, range: Range, selectionRange: Range, children: DocumentSymbol[]) {
        super(name, detail, kind, range, selectionRange)
        this.children = children
    }
}