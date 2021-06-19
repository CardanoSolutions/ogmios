import repl from 'repl'
import parser from 'yargs-parser'
import {
  ChainSyncMessageHandlers,
  ConnectionConfig,
  createChainSyncClient,
  createStateQueryClient,
  currentEpoch,
  currentProtocolParameters,
  delegationsAndRewards,
  eraStart,
  findIntersect,
  genesisConfig,
  getOgmiosHealth,
  ledgerTip,
  nonMyopicMemberRewards,
  proposedProtocolParameters,
  Schema,
  stakeDistribution,
  submitTx,
  utxo
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
  const chainSync = await createChainSyncClient({
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
  },
  console.error,
  { connection }
  )
  const cardanoOgmiosRepl = repl.start({
    prompt: 'ogmios> ',
    ignoreUndefined: true
  })

  Object.assign(cardanoOgmiosRepl.context, {
    chainSync,
    createChainSyncClient:
      (messageHandlers: ChainSyncMessageHandlers) =>
        createChainSyncClient(messageHandlers, console.error, { connection }),
    createStateQueryClient: () => createStateQueryClient(console.error, { connection }),
    currentEpoch: () => currentEpoch(connection),
    currentProtocolParameters: () => currentProtocolParameters(connection),
    delegationsAndRewards:
      (stakeKeyHashes: Schema.Hash16[]) => delegationsAndRewards(stakeKeyHashes, connection),
    eraStart: () => eraStart(connection),
    genesisConfig: () => genesisConfig(connection),
    getOgmiosHealth: () => getOgmiosHealth(connection),
    findIntersect: (points: Schema.Point[]) => findIntersect(points, connection),
    ledgerTip: () => ledgerTip(connection),
    nonMyopicMemberRewards:
      (input: (Schema.Lovelace | Schema.Hash16)[]) =>
        nonMyopicMemberRewards(input, connection),
    proposedProtocolParameters: () => proposedProtocolParameters(connection),
    stakeDistribution: () => stakeDistribution(connection),
    submitTx: (bytes: string) => submitTx(bytes, connection),
    utxo: (addresses?: Schema.Address[]) => utxo(addresses, connection)
  })

  cardanoOgmiosRepl.on('exit', async () => {
    await Promise.all([
      chainSync.shutdown
    ])
    process.exit(1)
  })
})()
