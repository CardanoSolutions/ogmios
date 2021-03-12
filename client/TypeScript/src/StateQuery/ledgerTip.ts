import { EraMismatch, Hash16, Ogmios, Point, Slot } from '../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../errors'
import { baseRequest } from '../Request'
import { ensureSocket, InteractionOptions } from '../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[ledgerTip]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonOriginPoint = (result: {slot: Slot, hash: Hash16}): result is {slot: Slot, hash: Hash16} =>
  (result as {slot: Slot, hash: Hash16}).slot !== undefined

export const ledgerTip = (options?: InteractionOptions): Promise<Point> => {
  return ensureSocket<Point>((socket) => {
    return new Promise((resolve, reject) => {
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[ledgerTip]'] = JSON.parse(message)
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('ledgerTip'))
        } else if (response.result === 'origin') {
          return resolve(response.result)
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else if (isNonOriginPoint(response.result)) {
          return resolve(response.result)
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: 'ledgerTip'
        }
      } as Ogmios['Query']))
    })
  },
  options
  )
}
