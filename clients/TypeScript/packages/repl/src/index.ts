import repl from 'repl'
import parser from 'yargs-parser'
import {
  ChainSync,
  ConnectionConfig,
  createConnectionObject,
  createChainSyncClient,
  createInteractionContext,
  getServerHealth,
  Schema,
  StateQuery,
  TxMonitor,
  TxSubmission
} from '@cardano-ogmios/client'
import chalk from 'chalk'
import util from 'util'

const log = console.log
const logObject = (obj: Object) =>
  log(util.inspect(obj, false, null, true));

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
  const chainSync = await createChainSyncClient(context, {
    rollBackward: async ({ point, tip }, requestNext) => {
      log(chalk.bgRedBright.bold('ROLL BACKWARD'))
      log(chalk.redBright.bold('Point'))
      logObject(point)
      log(chalk.redBright.bold('Tip'))
      logObject(tip)
      requestNext()
    },
    rollForward: async ({ block, tip }, requestNext) => {
      log(chalk.bgGreen.bold('ROLL FORWARD'))
      log(chalk.green.bold('Block'))
      logObject(block)
      log(chalk.green.bold('Tip'))
      logObject(tip)
      requestNext()
    }
  }
  )
  const cardanoOgmiosRepl = repl.start({
    prompt: `${args.host}> `,
    ignoreUndefined: true
  })

  Object.assign(cardanoOgmiosRepl.context, {
    chainSync,
    blockHeight: () => StateQuery.blockHeight(context),
    chainTip: () => StateQuery.chainTip(context),
    currentEpoch: () => StateQuery.currentEpoch(context),
    currentProtocolParameters: () => StateQuery.currentProtocolParameters(context),
    delegationsAndRewards:
      (stakeKeyHashes: Schema.DigestBlake2BCredential[]) => StateQuery.delegationsAndRewards(context, stakeKeyHashes),
    eraStart: () => StateQuery.eraStart(context),
    eraSummaries: () => StateQuery.eraSummaries(context),
    evaluateTx: (bytes: string, additionalUtxoSet?: Schema.Utxo) => TxSubmission.evaluateTx(context, bytes, additionalUtxoSet),
    genesisConfig: () => StateQuery.genesisConfig(context),
    getServerHealth: () => getServerHealth({ connection: createConnectionObject(connection) }),
    findIntersect: (points: Schema.Point[]) => ChainSync.findIntersect(context, points),
    ledgerTip: () => StateQuery.ledgerTip(context),
    nonMyopicMemberRewards:
      (input: (Schema.Lovelace | Schema.DigestBlake2BCredential)[]) =>
        StateQuery.nonMyopicMemberRewards(context, input),
    poolIds: () => StateQuery.poolIds(context),
    poolParameters: (pools: Schema.PoolId[]) => StateQuery.poolParameters(context, pools),
    poolsRanking: () => StateQuery.poolsRanking(context),
    proposedProtocolParameters: () => StateQuery.proposedProtocolParameters(context),
    rewardsProvenance: () => StateQuery.rewardsProvenance(context),
    rewardsProvenanceNew: () => StateQuery.rewardsProvenanceNew(context),
    stakeDistribution: () => StateQuery.stakeDistribution(context),
    systemStart: () => StateQuery.systemStart(context),
    submitTx: (bytes: string) => TxSubmission.submitTx(context, bytes),
    awaitAcquire: () => TxMonitor.awaitAcquire(context),
    hasTx: (id: Schema.TxId) => TxMonitor.hasTx(context, id),
    nextTx: (args: { fields?: 'all' } = {}) => TxMonitor.nextTx(context, args),
    sizeAndCapacity: () => TxMonitor.sizeAndCapacity(context),
    releaseMempool: () => TxMonitor.release(context),
    utxo: (filters?: Schema.Address[]|Schema.TxIn[]) => StateQuery.utxo(context, filters)
  })

  cardanoOgmiosRepl.on('exit', async () => {
    await Promise.all([
      chainSync.shutdown
    ])
    process.exit(1)
  })
})().catch((error) => {
  console.log(error)
  process.exit(1)
})
