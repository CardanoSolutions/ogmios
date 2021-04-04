import { nanoid } from 'nanoid'
import { Ogmios, Epoch, EraMismatch } from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[currentEpoch]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

export const currentEpoch = (context?: InteractionContext): Promise<Epoch> => {
  return ensureSocket<Epoch>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[currentEpoch]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('currentEpoch'))
        } else if (typeof response.result === 'number') {
          return resolve(response.result)
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: 'currentEpoch'
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
