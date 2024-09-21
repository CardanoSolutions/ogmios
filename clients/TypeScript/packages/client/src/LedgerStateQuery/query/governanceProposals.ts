import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  GovernanceProposalReference,
  GovernanceProposalState
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateGovernanceProposals']
type Response = Ogmios['QueryLedgerStateGovernanceProposalsResponse']

/**
 * Get currently active {@link GovernanceProposalState}, possibly filtered by {@link GovernanceProposalReference}.
 *
 * @category LedgerStateQuery
 */
export function governanceProposals (
  context: InteractionContext,
  proposals?: GovernanceProposalReference[]
): Promise<GovernanceProposalState[]> {
  return Method<Request, Response, GovernanceProposalState[]>(
    {
      method: 'queryLedgerState/governanceProposals',
      ...Array.isArray(proposals) ? { params: { proposals } } : {}
    },
    {},
    context
  )
}
