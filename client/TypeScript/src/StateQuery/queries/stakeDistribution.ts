import { nanoid } from 'nanoid'
import { EraMismatch, Ogmios, PoolDistribution } from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[stakeDistribution]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isPoolDistribution = (result: Ogmios['QueryResponse[stakeDistribution]']['result']): result is PoolDistribution =>
  Object.values(result as PoolDistribution)[0].stake !== undefined

export const stakeDistribution = (context?: InteractionContext): Promise<PoolDistribution> => {
  return ensureSocket<PoolDistribution>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[stakeDistribution]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('stakeDistribution'))
        } else if (isPoolDistribution(response.result)) {
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
          query: 'stakeDistribution'
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
