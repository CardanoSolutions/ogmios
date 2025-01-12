import {
  createInteractionContext,
  createConnectionObject,
  InteractionContext
} from '../src'

describe('Connection', () => {
  describe('createConnectionObject', () => {
    it('can be passed undefined', () => {
      expect(createConnectionObject(undefined)).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'http://127.0.0.1:1337',
          webSocket: 'ws://127.0.0.1:1337'
        }
      })
    })
    it('can be passed a complete ConnectionConfig object', () => {
      expect(createConnectionObject({
        host: 'some-host',
        port: 1337,
        tls: true
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'https://some-host:1337',
          webSocket: 'wss://some-host:1337'
        }
      })
    })
    it('can be passed an object with just a host', () => {
      expect(createConnectionObject({
        host: 'some-host'
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'http://some-host:1337',
          webSocket: 'ws://some-host:1337'
        }
      })
    })
    it('can be passed an object with just a port', () => {
      expect(createConnectionObject({
        port: 1337
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'http://127.0.0.1:1337',
          webSocket: 'ws://127.0.0.1:1337'
        }
      })
    })
    it('can be passed an object with just tls', () => {
      expect(createConnectionObject({
        tls: true
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'https://127.0.0.1:1337',
          webSocket: 'wss://127.0.0.1:1337'
        }
      })
    })
    it('can be passed an object excluding a host', () => {
      expect(createConnectionObject({
        port: 1337,
        tls: true
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'https://127.0.0.1:1337',
          webSocket: 'wss://127.0.0.1:1337'
        }
      })
    })
    it('can be passed an object excluding a port', () => {
      expect(createConnectionObject({
        host: '127.0.0.1',
        tls: true
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'https://127.0.0.1:1337',
          webSocket: 'wss://127.0.0.1:1337'
        }
      })
    })
    it('can be passed an object excluding tls', () => {
      expect(createConnectionObject({
        host: '127.0.0.1',
        port: 1337
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'http://127.0.0.1:1337',
          webSocket: 'ws://127.0.0.1:1337'
        }
      })
    })
    it('can be passed an address directly', () => {
      expect(createConnectionObject({
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })).toStrictEqual({
        maxPayload: 134217728,
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })
    })
    it('can be passed an address and maxPayload', () => {
      expect(createConnectionObject({
        maxPayload: 1024,
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })).toStrictEqual({
        maxPayload: 1024,
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })
    })
    it('cannot be passed an address and a host', () => {
      expect(() => createConnectionObject({
        host: '127.0.0.1',
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })).toThrow()
    })
    it('cannot be passed an address and a port', () => {
      expect(() => createConnectionObject({
        port: 1337,
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })).toThrow()
    })
    it('cannot be passed an address and a tls flag', () => {
      expect(() => createConnectionObject({
        tls: true,
        address: {
          http: 'http://127.0.0.1:1442',
          webSocket: 'ws://127.0.0.1:1442'
        }
      })).toThrow()
    })
  })
  describe('InteractionContext', () => {
    it('rejects with the Websocket errors on failed connection', async () => {
      let context: InteractionContext
      try {
        context = await createInteractionContext(
          (error) => console.error(error),
          () => {},
          { connection: { host: 'non-existent-host', port: 1111 } }
        )
        expect(context).toBeUndefined()
        if (context.socket.readyState === context.socket.OPEN) {
          await context.socket.close()
        }
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        context = await createInteractionContext(
          (error) => console.error(error),
          () => {},
          { connection: { port: 1111 } }
        )
        expect(context).toBeUndefined()
        if (context.socket.readyState === context.socket.OPEN) {
          await context.socket.close()
        }
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
})
