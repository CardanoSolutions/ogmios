module.exports = {
  globalSetup: './test/jest-puppeteer.global-setup.ts',
  preset: 'jest-puppeteer',
  setupFilesAfterEnv: ['<rootDir>/test/jest-puppeteer.setup.ts'],
  transform: {
    "^.+\\.ts?$": "ts-jest"
  },
  testMatch: ["**/?(*.)+(spec|test).[t]s"],
  testPathIgnorePatterns: ['/node_modules/', 'dist'],
  testTimeout: 120000
}
