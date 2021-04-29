import { nanoid } from 'nanoid'
import { CompactGenesis, EraMismatch, Ogmios } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[currentProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isGenesisConfig = (result: Ogmios['QueryResponse[genesisConfig]']['result']): result is CompactGenesis =>
  (result as CompactGenesis).systemStart !== undefined

export const genesisConfig = (context?: InteractionContext): Promise<CompactGenesis> => {
  return ensureSocket<CompactGenesis>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[genesisConfig]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('genesisConfig'))
        } else if (isGenesisConfig(response.result)) {
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
          query: 'genesisConfig'
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
