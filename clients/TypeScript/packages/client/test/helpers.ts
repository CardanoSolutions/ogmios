import { ConnectionConfig, createInteractionContext } from '../src'

const _512MB = 512 * 1024 * 1024

export const dummyInteractionContext = async (
  connection: ConnectionConfig = { maxPayload: _512MB }
) => {
  return createInteractionContext(
    console.error,
    () => {},
    {
      connection: { port: 1337, ...connection },
      maxEventListeners: 99
    }
  )
}
