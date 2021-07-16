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
    it('fetches the service metadata when passed a ConnectionConfig', async () => {
      expectHealth(await getOgmiosHealth({ port: 1338 }))
    })
    it('fetches the service metadata when passed a ConnectionObject', async () => {
      const connection = await createConnectionObject({ port: 1338 })
      expectHealth(await getOgmiosHealth(connection))
    })
    it('throws fetch errors if encountered', async () => {
      expect.assertions(1)
      try {
        await getOgmiosHealth({ host: 'non-existent' })
      } catch (error) {
        expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
    })
  })
})
