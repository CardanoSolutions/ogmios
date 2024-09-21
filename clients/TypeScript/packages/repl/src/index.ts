import repl from 'repl'
import parser from 'yargs-parser'
import {
  ConnectionConfig,
  LedgerStateQuery,
  MempoolMonitoring,
  Schema,
  TransactionSubmission,
  createConnectionObject,
  createInteractionContext,
  getServerHealth
} from '@cardano-ogmios/client'

(async () => {
  const args = parser(process.argv)
  const _512MB = 512 * 1024 * 1024
  const connection = {
    maxPayload: _512MB,
    host: args.host,
    port: args.port,
    tls: args.tls
  } as ConnectionConfig
  const context = await createInteractionContext(console.error, () => {}, { connection })

  const cardanoOgmiosRepl = repl.start({
    prompt: `${context.connection.address.webSocket}> `,
    ignoreUndefined: true
  })

  Object.assign(cardanoOgmiosRepl.context, {
    acquireMempool: () => MempoolMonitoring.acquireMempool(context),
    constitution: () => LedgerStateQuery.constitution(context),
    epoch: () => LedgerStateQuery.epoch(context),
    eraStart: () => LedgerStateQuery.eraStart(context),
    eraSummaries: () => LedgerStateQuery.eraSummaries(context),
    evaluateTransaction: (
      bytes: string,
      additionalUtxoSet?: Schema.Utxo
    ) => TransactionSubmission.evaluateTransaction(context, bytes, additionalUtxoSet),
    genesisConfiguration: (
      era: Schema.EraWithGenesis
    ) => LedgerStateQuery.genesisConfiguration(context, era as 'alonzo'),
    getServerHealth: () => getServerHealth({ connection: createConnectionObject(connection) }),
    governanceProposals: (
      filters?: Schema.GovernanceProposalReference[]
    ) => LedgerStateQuery.governanceProposals(context, filters),
    hasTransaction: (
      id: Schema.TransactionId
    ) => MempoolMonitoring.hasTransaction(context, id),
    ledgerTip: () => LedgerStateQuery.ledgerTip(context),
    liveStakeDistribution: () => LedgerStateQuery.liveStakeDistribution(context),
    networkBlockHeight: () => LedgerStateQuery.networkBlockHeight(context),
    networkStartTime: () => LedgerStateQuery.networkStartTime(context),
    networkTip: () => LedgerStateQuery.networkTip(context),
    nextTransaction: (
      args?: { fields: 'all' }
    ) => MempoolMonitoring.nextTransaction(context, args),
    projectedRewards: (filters?: {
      stake?: Schema.ValueAdaOnly[],
      scripts?: Schema.AnyStakeCredential[],
      keys?: Schema.AnyStakeCredential[],
    }) => LedgerStateQuery.projectedRewards(context, filters),
    proposedProtocolParameters: () => LedgerStateQuery.proposedProtocolParameters(context),
    protocolParameters: () => LedgerStateQuery.protocolParameters(context),
    releaseMempool: () => MempoolMonitoring.releaseMempool(context),
    rewardAccountSummaries: (filters?: {
      scripts?: Schema.AnyStakeCredential[],
      keys?: Schema.AnyStakeCredential[]
    }) => LedgerStateQuery.rewardAccountSummaries(context, filters),
    rewardsProvenance: () => LedgerStateQuery.rewardsProvenance(context),
    sizeOfMempool: () => MempoolMonitoring.sizeOfMempool(context),
    stakePools: () => LedgerStateQuery.stakePools(context),
    submitTransaction: (
      bytes: string
    ) => TransactionSubmission.submitTransaction(context, bytes),
    utxo: (
      filters?: Schema.UtxoByAddresses | Schema.UtxoByOutputReferences
    ) => LedgerStateQuery.utxo(context, filters)
  })

  cardanoOgmiosRepl.on('exit', async () => {
    process.exit(1)
  })
})().catch((error) => {
  console.log(error)
  process.exit(1)
})
