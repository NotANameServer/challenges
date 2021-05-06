const { resolve } = require('path')

module.exports = {
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html'),
        level1: resolve(__dirname, 'level1/index.html'),
        level2: resolve(__dirname, 'level2/index.html'),
        level3: resolve(__dirname, 'level3/index.html'),
      }
    }
  }
}