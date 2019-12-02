import * as vscode from 'vscode'
import * as CompileNetCommand from './CompileNetCommand'
import * as ViewNetCommand from './ViewNetCommand'
import { NetDocumentSymbolProvider } from './NetDocumentSymbolProvider';

export function activate(context: vscode.ExtensionContext) {
	console.log('Hi there! The Statebox extension is now active.')
	let compileNetCommandSub = vscode.commands.registerCommand('extension.compileNet', CompileNetCommand.compileStbxUI(context))
	let viewNetCommandSub = vscode.commands.registerCommand('extension.viewNet', ViewNetCommand.viewNet(context))

	context.subscriptions.push(compileNetCommandSub)
	context.subscriptions.push(viewNetCommandSub)

	context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(
		{language: "statebox"}, new NetDocumentSymbolProvider()
		// "statebox", new NetDocumentSymbolProvider()
	))
}

export function deactivate() {}