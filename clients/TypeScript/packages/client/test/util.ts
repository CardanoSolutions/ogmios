import {
  ConnectionConfig,
  createInteractionContext,
  InteractionType
} from '../src'

const _512MB = 512 * 1024 * 1024

export const dummyInteractionContext = async (
  interactionType: InteractionType = 'LongRunning',
  connection: ConnectionConfig = { maxPayload: _512MB }
) => {
  return createInteractionContext(
    console.error,
    () => {},
    { connection: { port: 1338, ...connection }, interactionType }
  )
}
