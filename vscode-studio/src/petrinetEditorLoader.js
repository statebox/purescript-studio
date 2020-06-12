// some 'imports' from our PureScript code
const PetrinetEditor = PS["View.Petrinet.JsApi"]

// some data passed down from the extension's webview
const config = window.petrinetEditorWebviewConfig

// acquire only once, then keep in local scope for security reasons
const vscode = acquireVsCodeApi()

console.log('dagre =', window.dagre)

if (!config.netModel) {
    console.error('Failed to retrieve the initial model from the values injected to the webview page window.')
} else {
    let netModel = config.netModel

    // TODO: temp fix for disappeared Maybe; we should serialise and deserialise this
    let Maybe = PS["Data.Maybe"], Nothing = PS["Data.Maybe"].Nothing, Just = PS["Data.Maybe"].Just
    netModel.placeNames = new Just(netModel.placeNames.value0)

    console.log('PetrinetEditor =', PetrinetEditor)
    const runner = PetrinetEditor.runPetrinetEditorUI_NLL("#petrinetEditor-container")("my_vscode_net")(netModel)
    runner()
}
