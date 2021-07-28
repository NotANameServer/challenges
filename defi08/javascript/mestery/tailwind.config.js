module.exports = {
  purge: {
    enabled: true,
    mode: 'all',
    content: ['./index.html', './src/**/*.js'],
    safelist: [
      'px-5',
    ]
  },
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      height: {
        '38vw': '38vw'
      },
      width: {
        '38vw': '38vw'
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
