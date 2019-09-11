# kd-Tree MonCat

[![Netlify Status](https://api.netlify.com/api/v1/badges/683fd161-cc3e-42dc-84af-7b77259af156/deploy-status)](https://app.netlify.com/sites/cranky-goldstine-6a507b/deploys)


## setup

    npm install

## usage

    npm run watch

Then open `dist/index.html` in your browser

## about

monoidal category generated from a binary tree of
- compose :: (b->c, a->b) => a->c
- tensor :: (a->b, c->d) => (a*c -> b*d)
- unit :: 1 -> 1

we use kd-tree-ish algo to divide the diagram of boxes into a suitable tree representing the diagram
