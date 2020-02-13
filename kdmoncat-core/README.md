# Statebox Monoidal Category Logic

## setup

```
npm install
```

## bundling

```
npm run bundle
```

This generates a static version in `dist/index.html`.

## alternative bundling

Alternatively, bundle using `rollup`

```
npm run rollup-bundle
```

The output is placed in `js-bundle/kdmoncat-bundle.min.js`.

Usage from Node JS:

```javascript
let context = `
f: a ->
g: b b b -> a c d
h: c d d -> e
i: b -> e d
j: [2 3 1 4]
k: -> e e
l: e e e -> e
`.trim()

let pixels = `
gggff
ggghh
ijjhh
kjjll
`.trim()

let conf = {pixels, context}

let KDMonCat = require('./js-bundle/kdmoncat-bundle.min.js')

async function main () {
    let r = await KDMonCat(conf)
    console.log(r) // morphism
}

main()
```