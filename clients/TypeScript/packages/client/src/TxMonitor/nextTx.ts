/* eslint no-redeclare: "off" */

import { Ogmios, TxAlonzo, TxId } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

/**
 * Request the next mempool transaction from an acquired snapshot.
 *
 * @category TxMonitor
 */
export function nextTx (context: InteractionContext, args?: { fields: 'all' }) : Promise<TxAlonzo|null>
export function nextTx (context: InteractionContext, args?: {}) : Promise<TxId|null>
export function nextTx (context: InteractionContext, args?: { fields: 'all' }) : Promise<TxId|TxAlonzo|null> {
  return Query<
        Ogmios['NextTx'],
        Ogmios['NextTxResponse'],
        TxAlonzo | null
    >({
      methodName: 'NextTx',
      args: args
    }, {
      handler: (response, resolve, reject) => {
        try {
          resolve(handleNextTxResponse(response, args))
        } catch (e) {
          reject(e)
        }
      }
    }, context)
}

/**
 * @internal
 */
export const isNextTxResultId = (result: any): result is TxId | null =>
  ((result as null) === null) || (typeof (result as TxId) === 'string')

/**
 * @internal
 */
export const isNextTxResultAll = (result: any): result is TxAlonzo | null =>
  ((result as null) === null) || (typeof (result as TxAlonzo) === 'object')

/**
 * @internal
 */
export function handleNextTxResponse (response: Ogmios['NextTxResponse'], args?: { fields: 'all' }): (TxAlonzo | null)
export function handleNextTxResponse (response: Ogmios['NextTxResponse'], args?: {}): (TxId | null)
export function handleNextTxResponse (response: Ogmios['NextTxResponse'], args?: { fields: 'all' }): (TxId | TxAlonzo | null) {
  const { result } = response

  if (args.fields === 'all') {
    if (isNextTxResultAll(result)) {
      return result
    }
  } else {
    if (isNextTxResultId(result)) {
      return result
    }
  }

  throw new UnknownResultError(response)
}
