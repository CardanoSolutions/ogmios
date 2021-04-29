const { setup: setupPuppeteer } = require('jest-environment-puppeteer')

module.exports = async function globalSetup (globalConfig: any) {
  await setupPuppeteer(globalConfig)
}
