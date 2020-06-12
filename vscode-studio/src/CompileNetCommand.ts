import * as vscode from 'vscode'
import * as path from 'path'
import {PS} from './PureScriptApi'

export function compileStbxUI(vscodeContext: vscode.ExtensionContext) {
  return () => compileStbxUIImpl(vscodeContext)
}

function compileStbxUIImpl(vscodeContext: vscode.ExtensionContext) {
  let activeEditor = vscode.window.activeTextEditor

  // TODO check that file type is stbx, and if we're in a stbx viewer, try to get the corresponding text editor
  if (activeEditor) {
    let src = activeEditor.document.getText()
    let astE = PS.Parser.parseNet(src)
    console.log('AST = ' + JSON.stringify(astE, null, 2))

    let parseResult = PS.Either.either((e: any) => e)(PS.Generator.mkParseResult)(astE)
    console.log('parseResultE = ' + JSON.stringify(parseResult, null, 2))

    let netModelE = PS.Either.either((e: any) => e)(PS.Generator_Net.toNetWithDefaultName("Petri net (TODO)"))(astE)
    console.log('netModelE = ' + JSON.stringify(netModelE, null, 2))

    let symbolModel = PS.Generator.mkTransitionSymbols(parseResult)
    console.log('symbolModel = ' + JSON.stringify(symbolModel, null, 2))

    let newEditorContent = JSON.stringify({ netModelE, parseResultE: parseResult, symbolModel }, null, 2)
    //console.log('newEditorContent = ' + newEditorContent)

    let fileName = activeEditor.document.fileName
    let timestamp = Date.now()
    let newFile = vscode.Uri.parse('untitled:' + fileName + '.' + timestamp + '.json')
    vscode.workspace.openTextDocument(newFile).then(doc => {
      let edit = new vscode.WorkspaceEdit()
      edit.insert(newFile, new vscode.Position(0, 0), newEditorContent)
      return vscode.workspace.applyEdit(edit).then(ok => {
        if (ok) {
          vscode.window.showTextDocument(doc)
        } else {
          vscode.window.showInformationMessage('Error showing generated code.')
        }
      })
    })
  } else {
    vscode.window.showInformationMessage('No stbx editor active.')
  }
}