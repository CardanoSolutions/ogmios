import { EraMismatch, Hash16, Ogmios, Point, Slot } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[ledgerTip]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonOriginPoint = (result: {slot: Slot, hash: Hash16}): result is {slot: Slot, hash: Hash16} =>
  (result as {slot: Slot, hash: Hash16}).slot !== undefined

export const ledgerTip = (context?: InteractionContext): Promise<Point> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[ledgerTip]'],
    Point
  >({
    methodName: 'Query',
    args: {
      query: 'ledgerTip'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('ledgerTip'))
      } else if (response.result === 'origin') {
        resolve(response.result)
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        reject(new EraMismatchError(queryEra, ledgerEra))
      } else if (isNonOriginPoint(response.result)) {
        return resolve(response.result)
      } else {
        reject(new UnknownResultError(response.result))
      }
    }
  }, context)
