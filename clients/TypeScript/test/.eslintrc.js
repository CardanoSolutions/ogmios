module.exports = {
  "extends": ["../.eslintrc.js"],
  "plugins": [
    "jest"
  ],
  "globals": {
    "it": "readonly",
    "before": "readonly",
    "after": "readonly",
    "describe": "readonly",
    "beforeEach": "readonly",
    "afterEach": "readonly"
  },
  "env": {
    "jest": true
  }
}