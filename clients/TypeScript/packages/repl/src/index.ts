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
  TxSubmission
} from '@cardano-ogmios/client'
import chalk from 'chalk'
import util from 'util'

const log = console.log
const logObject = (obj: Object) =>
  log(util.inspect(obj, false, null, true));

(async () => {
  const args = parser(process.argv)
  const connection = {
    host: args.host,
    port: args.port
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
    prompt: 'ogmios> ',
    ignoreUndefined: true
  })

  Object.assign(cardanoOgmiosRepl.context, {
    chainSync,
    currentEpoch: () => StateQuery.currentEpoch(context),
    currentProtocolParameters: () => StateQuery.currentProtocolParameters(context),
    delegationsAndRewards:
      (stakeKeyHashes: Schema.Hash16[]) => StateQuery.delegationsAndRewards(context, stakeKeyHashes),
    eraStart: () => StateQuery.eraStart(context),
    genesisConfig: () => StateQuery.genesisConfig(context),
    getServerHealth: () => getServerHealth(createConnectionObject(connection)),
    findIntersect: (points: Schema.Point[]) => ChainSync.findIntersect(context, points),
    ledgerTip: () => StateQuery.ledgerTip(context),
    nonMyopicMemberRewards:
      (input: (Schema.Lovelace | Schema.Hash16)[]) =>
        StateQuery.nonMyopicMemberRewards(context, input),
    proposedProtocolParameters: () => StateQuery.proposedProtocolParameters(context),
    stakeDistribution: () => StateQuery.stakeDistribution(context),
    submitTx: (bytes: string) => TxSubmission.submitTx(context, bytes),
    utxo: (filters?: Schema.Address[]|Schema.TxIn[]) => StateQuery.utxo(context, filters)
  })

  cardanoOgmiosRepl.on('exit', async () => {
    await Promise.all([
      chainSync.shutdown
    ])
    process.exit(1)
  })
})()
