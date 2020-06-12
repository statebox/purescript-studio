# Statebox vscode extension

## Features

- Draw Petri nets using a simple ASCII syntax.
- Compile the nets.
- Render the nets visually.

## Installation and running

- Install dependencies: `npm i`.
- Build the extension: `npm run vscode:publish`
- Open the project in vscode.

## Usage

Once you built the extension as described above, you can run it by pressing `F5`. (Press it twice if necessary. Also, if necessary ignore warnings about alleged errors in the PureScript and run anyway.) Then, from the new vscode instance that opens, running the Statebox extension, you can open a `.stbx` file. Say for example such a file contains a Petri net specification of some traffic lights:
```
t1: green1       -> red1, queue
t2: red1,  queue -> green1
t3: queue, red2  -> green2
t4: green2       -> queue, red2
```
Once you have this open, use `Cmd-shift-p` (on Mac) to open the vscode command pallette. Search for 'Statebox' and it should give you some command to run om the net.