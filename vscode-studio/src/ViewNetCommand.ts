import * as vscode from 'vscode'
import * as path from 'path'
import {PS, AST, NetModel} from './PureScriptApi'

export const viewNet = (context: vscode.ExtensionContext) =>
  () => {
    let panel = vscode.window.createWebviewPanel('extension', 'Petri net viewer', vscode.ViewColumn.Beside, { enableScripts: true })

    const bundleUri = panel.webview.asWebviewUri(vscode.Uri.file(path.join(context.extensionPath, 'bundle', 'halogen-petrinet-editor-bundle.js')))
    const dagreUri = panel.webview.asWebviewUri(vscode.Uri.file(path.join(context.extensionPath, 'bundle', 'dagre.min.js')))
    const cssUri = panel.webview.asWebviewUri(vscode.Uri.file(path.join(context.extensionPath, 'bundle', 'petrinet.css')))
    const cssPatchUri = panel.webview.asWebviewUri(vscode.Uri.file(path.join(context.extensionPath, 'src', 'petrinet-patch.css')))
    const loaderUri = panel.webview.asWebviewUri(vscode.Uri.file(path.join(context.extensionPath, 'src', 'petrinetEditorLoader.js')))

    if (vscode.window.activeTextEditor) {
      let updatedSourceText = vscode.window.activeTextEditor.document.getText()
      updateWebview(updatedSourceText)
    } else {
      vscode.window.showWarningMessage('No text editor active to update the net from.')
    }

    vscode.workspace.onDidChangeTextDocument(change => updateWebview(change.document.getText()))
    vscode.workspace.onDidOpenTextDocument(edit => updateWebview(edit.getText()))

    function toWebviewContent(net: NetModel) {
      return `<!DOCTYPE html>
      <html lang="en">
      <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <meta
            http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${panel.webview.cspSource} https:; script-src 'unsafe-inline' ${panel.webview.cspSource}; style-src 'unsafe-inline' ${panel.webview.cspSource};"
          />
          <title>Statebox net viewer</title>
          <script>
            // inject these values into the page and window so the scripts can access them
            window.petrinetEditorWebviewConfig = {
              netModel: ${JSON.stringify(net)}
            }
          </script>
          <script src="${dagreUri}"></script>
          <script src="${bundleUri}"></script>
          <link href="${cssUri}" rel="stylesheet">
          <link href="${cssPatchUri}" rel="stylesheet">
      </head>
      <body>
          <div id="petrinetEditor-container"></div>
          <script src="${loaderUri}"></script>
      </body>
      </html>`
    }

    function updateWebview(src: string) {
      console.log('parsing...')
      let astE = PS.Parser.parseNet(src)
      PS.Either.either((e: any) => {
        vscode.window.showWarningMessage('Error parsing net: ' + e)
      })((ast: AST) => {
        console.log('compiling...')
        let netModelE = PS.Generator_Net.toNetWithDefaultName("dummy net name")(ast)
        PS.Either.either((e: any) => {
          vscode.window.showWarningMessage("Could not visualise net because of a problem compiling the net.")
          return Promise.reject("Failed to fold ParseResultE.")
        })((netModel: NetModel) => {
          // console.log('compiled net = ' + JSON.stringify(netModel, null, 2))
          panel.webview.html = toWebviewContent(netModel)
          return netModel
        })(netModelE)
      })(astE)
    }
}