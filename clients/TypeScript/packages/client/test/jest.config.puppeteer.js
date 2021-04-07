const { pathsToModuleNameMapper } = require('ts-jest/utils')
const { compilerOptions } = require('./tsconfig')

module.exports = {
  globalSetup: './jest-puppeteer.global-setup.ts',
  moduleNameMapper: pathsToModuleNameMapper(compilerOptions.paths),
  preset: 'jest-puppeteer',
  setupFilesAfterEnv: ['<rootDir>/jest-puppeteer.setup.ts'],
  transform: {
    "^.+\\.ts?$": "ts-jest"
  },
  testMatch: ["**/?(*.)+(spec|test).[t]s"],
  testPathIgnorePatterns: ['/node_modules/', 'dist'],
  testTimeout: 120000
}
