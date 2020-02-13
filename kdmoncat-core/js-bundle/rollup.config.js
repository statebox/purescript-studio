import {terser} from 'rollup-plugin-terser'
import resolve from '@rollup/plugin-commonjs'

export default {
  input: 'js-bundle/bundle-entrypoint.js',
  output: [
    {
      file: 'js-bundle/kdmoncat-bundle.min.js',
      format: 'umd',
      name: 'KDMonCat',
      plugins: [
          terser({
            mangle: true,
            compress: true
          })
      ]
    }
  ],
  plugins: [ resolve() ]
};
