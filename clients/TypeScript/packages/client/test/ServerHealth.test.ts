import { createConnectionObject, getServerHealth } from '../src'

const expectHealth = (obj: any): void => {
  const keys = Object.keys(obj)
  expect(keys)
    .toEqual(expect.arrayContaining([
      'currentEra',
      'lastKnownTip',
      'lastTipUpdate',
      'metrics',
      'startTime',
      'networkSynchronization',
      'currentEpoch',
      'slotInEpoch'
    ]))
  expect(obj.currentEpoch).not.toBe(null)
  expect(obj.slotInEra).not.toBe(null)
}

describe('ServerHealth', () => {
  describe('getServerHealth', () => {
    // This test requires a mainnet instance, so is currently disabled.
    // it('fetches the service metadata using default connection config by default', async () => {
    //   expectHealth(await getServerHealth())
    // })
    it('fetches the service metadata', async () => {
      const connection = createConnectionObject({ port: 1338 })
      expectHealth(await getServerHealth({ connection }))
    })
    it('throws fetch errors if encountered', async () => {
      expect.assertions(1)
      try {
        const connection = createConnectionObject({ host: 'non-existent' })
        await getServerHealth({ connection })
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
    })
  })
})
