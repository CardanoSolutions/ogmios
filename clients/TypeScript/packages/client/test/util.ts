import {
  ConnectionConfig,
  createInteractionContext,
  InteractionType
} from '../src'

export const dummyInteractionContext = async (
  interactionType: InteractionType = 'LongRunning',
  connection: ConnectionConfig = {}
) => {
  return createInteractionContext(
    console.error,
    () => {},
    { connection: { port: 1338, ...connection }, interactionType }
  )
}
