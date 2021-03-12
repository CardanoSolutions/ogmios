import { Ogmios, Bound } from '../../schema'
import { QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isBound = (result: Ogmios['QueryResponse[eraStart]']['result']): result is Bound => {
  const bound = result as Bound
  return bound.time !== undefined && bound.slot !== undefined && bound.epoch !== undefined
}

export const eraStart = (context?: InteractionContext): Promise<Bound> => {
  return ensureSocket<Bound>((socket) => {
    return new Promise((resolve, reject) => {
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[eraStart]'] = JSON.parse(message)
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('ledgerTip'))
        } else if (isBound(response.result)) {
          return resolve(response.result)
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: 'eraStart'
        }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
