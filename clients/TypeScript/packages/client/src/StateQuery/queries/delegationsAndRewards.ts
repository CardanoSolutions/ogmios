import { nanoid } from 'nanoid'
import {
  DelegationsAndRewards1,
  EraMismatch, Hash16,
  Ogmios
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[delegationsAndRewards]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isDelegationsAndRewards = (result: Ogmios['QueryResponse[delegationsAndRewards]']['result']): result is DelegationsAndRewards1 => {
  const sample = Object.entries(result as DelegationsAndRewards1)[0]
  return typeof sample[0] === 'string' && (sample[1].delegate !== undefined || sample[1].rewards !== undefined)
}

export const delegationsAndRewards = (stakeKeyHashes: Hash16[], context?: InteractionContext): Promise<DelegationsAndRewards1> => {
  return ensureSocket<DelegationsAndRewards1>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[delegationsAndRewards]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('delegationsAndRewards'))
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else if (isDelegationsAndRewards(response.result)) {
          return resolve(response.result)
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: { delegationsAndRewards: stakeKeyHashes }
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
