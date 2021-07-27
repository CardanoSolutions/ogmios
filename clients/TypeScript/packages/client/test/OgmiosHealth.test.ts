import { getOgmiosHealth } from '@src/OgmiosHealth'
import { createConnectionObject } from '@src/Connection'

const expectHealth = (obj: any): void => {
  const keys = Object.keys(obj)
  expect(keys)
    .toEqual(expect.arrayContaining([
      'currentEra',
      'lastKnownTip',
      'lastTipUpdate',
      'metrics',
      'startTime',
      'networkSynchronization'
    ]))
}

describe('OgmiosHealth', () => {
  describe('getOgmiosHealth', () => {
    // This test requires a mainnet instance, so is currently disabled.
    // it('fetches the service metadata using default connection config by default', async () => {
    //   expectHealth(await getOgmiosHealth())
    // })
    it('fetches the service metadata', async () => {
      const connection = createConnectionObject({ port: 1338 })
      expectHealth(await getOgmiosHealth(connection))
    })
    it('throws fetch errors if encountered', async () => {
      expect.assertions(1)
      try {
        const connection = createConnectionObject({ host: 'non-existent' })
        await getOgmiosHealth(connection)
      } catch (error) {
        expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
    })
  })
})
