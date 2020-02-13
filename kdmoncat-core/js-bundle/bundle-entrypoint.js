let api = require('../output/index.js')

module.exports = ({pixels, context}) => {
  return new Promise((resolve, reject) => {
    try{
      let result = api.toJSON({pixels, context})()
      resolve(result)
    } catch(e) {
      reject(e)
    }
  })
}