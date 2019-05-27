# purescript-stbx-core

Testing:
```
npm install
npm run test
```

**Note:** test execution will currently fail with the following message:
```
/Users/erik/dev/statebox/studio-155-tests/stbx-core/output/Statebox.Core.Execution/foreign.js:6
  return new Stbx(wiring)
  ^

ReferenceError: Stbx is not defined
    at Object.fromWiring (/Users/erik/dev/statebox/studio-155-tests/stbx-core/output/Statebox.Core.Execution/foreign.js:6:3)
```
This is because the `stbx-core-js` bundle is not yet included when executing tests. See [studio issue #168](https://github.com/statebox/purescript-studio/issues/168).
