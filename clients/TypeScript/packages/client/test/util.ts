import { ConnectionConfig, createConnectionObject, InteractionContext } from '@src/Connection'

export const expectContextFromConnectionConfig = (connectionConfig: ConnectionConfig, context: InteractionContext) => {
  const connection = createConnectionObject(connectionConfig)
  expect(context.connection.address.webSocket).toBe(connection.address.webSocket)
  expect(context.socket.readyState).toBe(context.socket.OPEN)
}
