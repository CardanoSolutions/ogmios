import { InteractionContext, Method } from '../../Connection'
import { Ogmios, Origin, Point } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateTip']
type Response = Ogmios['QueryLedgerStateTipResponse']

/**
 * Get the current ledger {@link Tip}. Note that may be different from the
 * network's tip as it depends on the underlying node the client is connected
 * too. That node may still be catching up with the network and the ledger tip
 * thus corresponds to the last header that was processed and added to the
 * ledger local chain.
 *
 * @category LedgerStateQuery
 */
export function ledgerTip (context: InteractionContext): Promise<Point | Origin> {
  return Method<Request, Response, Point | Origin>(
    {
      method: 'queryLedgerState/tip'
    },
    {},
    context
  )
}
