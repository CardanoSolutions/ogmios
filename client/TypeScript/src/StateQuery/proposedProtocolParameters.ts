import { EraMismatch, Ogmios, ProtocolParametersShelley } from '../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError } from '../errors'
import { baseRequest } from '../Request'
import { ensureSocket, InteractionContext } from '../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

export const proposedProtocolParameters = (context?: InteractionContext): Promise<{[k: string]: ProtocolParametersShelley}> => {
  return ensureSocket<{[k: string]: ProtocolParametersShelley}>((socket) => {
    return new Promise((resolve, reject) => {
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[proposedProtocolParameters]'] = JSON.parse(message)
        if (response === undefined) return
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('proposedProtocolParameters'))
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else {
          return resolve(response.result)
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: 'proposedProtocolParameters'
        }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
