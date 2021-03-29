import {
  Address,
  EraMismatch,
  Ogmios,
  Utxo,
  UtxoMary
} from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[utxo]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isArrayOfUtxo = (result: Ogmios['QueryResponse[utxo]']['result']): result is (Utxo | UtxoMary)[] => {
  if (!Array.isArray(result)) {
    return false
  } else if (Array.isArray(result) && result.length === 0) {
    return true
  }
  const item = result[0] as (Utxo | UtxoMary)
  return Array.isArray(item) && item.length === 0 ||
    'index' in item[0] ||
    ('index' in item[0] &&
      (typeof item[1].value === 'number' || typeof item[1].value.coins === 'number'))
}

export const utxo = (addresses?: Address[], context?: InteractionContext): Promise<(Utxo | UtxoMary)[]> => {
  return ensureSocket<(Utxo | UtxoMary)[]>((socket) => {
    return new Promise((resolve, reject) => {
      socket.once('message', (message: string) => {
        const { result }: Ogmios['QueryResponse[utxo]'] = JSON.parse(message)
        if (result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('utxo'))
        } else if (isEraMismatch(result)) {
          const { eraMismatch } = result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else if (isArrayOfUtxo(result)) {
          return resolve(result)
        } else {
          return reject(new UnknownResultError(result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: addresses !== undefined ? { utxo: addresses } : 'utxo'
        }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
