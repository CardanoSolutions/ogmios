import { nanoid } from 'nanoid'
import { EraMismatch, Ogmios, ProtocolParametersShelley } from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

export interface ObjectOfProtocolParametersShelley { [k: string]: ProtocolParametersShelley }

const isEraMismatch = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isObjectOfProtocolParametersShelley = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is ObjectOfProtocolParametersShelley =>
  Object.values(result as ObjectOfProtocolParametersShelley)[0].minFeeCoefficient !== undefined

export const proposedProtocolParameters = (context?: InteractionContext): Promise<ObjectOfProtocolParametersShelley> => {
  return ensureSocket<{[k: string]: ProtocolParametersShelley}>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[proposedProtocolParameters]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('proposedProtocolParameters'))
        } else if (isObjectOfProtocolParametersShelley(response.result)) {
          return resolve(response.result)
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
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
