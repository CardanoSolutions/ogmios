module.exports = {
  "parser": "@typescript-eslint/parser",
  "extends": [
    "standard"
  ],
  "plugins": [
    "@typescript-eslint"
  ],
  "globals": {
    "AsyncGenerator": true
  },
  "rules": {
    "no-unused-vars": 0,
    "linebreak-style": [
      2,
      "unix"
    ],
    "no-unused-expressions": 0,
    "no-useless-constructor": 0,
    "@typescript-eslint/no-floating-promises": 1
  }
}
