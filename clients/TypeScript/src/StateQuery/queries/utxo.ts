import { nanoid } from 'nanoid'
import {
  Address,
  EraMismatch,
  Ogmios,
  Utxo1,
  Utxo2,
  UtxoMary
} from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[utxo]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isArrayOfUtxo = (result: Ogmios['QueryResponse[utxo]']['result']): result is Utxo1 => {
  if (!Array.isArray(result)) {
    return false
  } else if (Array.isArray(result) && result.length === 0) {
    return true
  }
  const item = result[0] as (Utxo2 | UtxoMary)
  return Array.isArray(item) && item.length === 0 ||
    'index' in item[0] ||
    ('index' in item[0] &&
      (typeof item[1].value === 'number' || typeof item[1].value.coins === 'number'))
}

export const utxo = (addresses?: Address[], context?: InteractionContext): Promise<Utxo1> => {
  return ensureSocket<Utxo1>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[utxo]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('utxo'))
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else if (isArrayOfUtxo(response.result)) {
          return resolve(response.result)
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: addresses !== undefined ? { utxo: addresses } : 'utxo'
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
