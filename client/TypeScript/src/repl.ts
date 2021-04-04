import repl from 'repl'
import parser from 'yargs-parser'
import {
  createStateQueryClient,
  currentEpoch,
  currentProtocolParameters,
  eraStart,
  ledgerTip,
  nonMyopicMemberRewards,
  proposedProtocolParameters,
  stakeDistribution,
  utxo
} from './StateQuery'
import {
  createChainSyncClient,
  findIntersect
} from './ChainSync'
import { submitTx } from './TxSubmission'
import chalk from 'chalk'
import util from 'util'
import { Address, Hash16, Lovelace, Point } from './schema'

const log = console.log
const logObject = (obj: Object) =>
  log(util.inspect(obj, false, null, true));

(async () => {
  const args = parser(process.argv)
  const connection = {
    host: args.host,
    port: args.port
  }
  const chainSync = await createChainSyncClient({ connection })
  chainSync.on({
    rollBackward: ({ point, tip, reflection }) => {
      log(chalk.bgRedBright.bold('ROLL BACKWARD'))
      log(chalk.redBright.bold('Point'))
      logObject(point)
      log(chalk.redBright.bold('Tip'))
      logObject(tip)
      if (reflection !== null) {
        log(chalk.redBright.bold('Reflection'))
        logObject(reflection)
      }
    },
    rollForward: ({ block, tip, reflection }) => {
      log(chalk.bgGreen.bold('ROLL FORWARD'))
      log(chalk.green.bold('Block'))
      logObject(block)
      log(chalk.green.bold('Tip'))
      logObject(tip)
      if (reflection !== null) {
        log(chalk.green.bold('Reflection'))
        logObject(reflection)
      }
    }
  })
  const cardanoOgmiosRepl = repl.start({
    prompt: 'ogmios> ',
    ignoreUndefined: true
  })

  Object.assign(cardanoOgmiosRepl.context, {
    chainSync,
    createChainSyncClient: () => createChainSyncClient({ connection }),
    createStateQueryClient: () => createStateQueryClient({ connection }),
    currentEpoch: () => currentEpoch({ connection }),
    currentProtocolParameters: () => currentProtocolParameters({ connection }),
    eraStart: () => eraStart({ connection }),
    findIntersect: (points: Point[]) => findIntersect(points, { connection }),
    ledgerTip: () => ledgerTip({ connection }),
    nonMyopicMemberRewards:
      (input: (Lovelace | Hash16)[]) => nonMyopicMemberRewards(input, { connection }),
    proposedProtocolParameters: () => proposedProtocolParameters({ connection }),
    stakeDistribution: () => stakeDistribution({ connection }),
    submitTx: (bytes: string) => submitTx(bytes, { connection }),
    utxo: (addresses?: Address[]) => utxo(addresses, { connection })
  })

  cardanoOgmiosRepl.on('exit', async () => {
    await Promise.all([
      chainSync.shutdown
    ])
    process.exit(1)
  })
})()
