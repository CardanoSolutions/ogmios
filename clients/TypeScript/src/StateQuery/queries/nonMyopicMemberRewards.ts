import { nanoid } from 'nanoid'
import { EraMismatch, Hash16, Lovelace, NonMyopicMemberRewards1, Ogmios } from '../../schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { baseRequest } from '../../Request'
import { ensureSocket, InteractionContext } from '../../Connection'

const isEraMismatch = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonMyopicMemberRewards = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is NonMyopicMemberRewards1 =>
  typeof Object.values(
    Object.values(result as NonMyopicMemberRewards1)[0]
  )[0] === 'number'

export const nonMyopicMemberRewards = (input: (Lovelace | Hash16)[], context?: InteractionContext): Promise<NonMyopicMemberRewards1> => {
  return ensureSocket<NonMyopicMemberRewards1>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['QueryResponse[nonMyopicMemberRewards]'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('nonMyopicMemberRewards'))
        } else if (isNonMyopicMemberRewards(response.result)) {
          return resolve(response.result)
        } else if (isEraMismatch(response.result)) {
          const { eraMismatch } = response.result
          const { ledgerEra, queryEra } = eraMismatch
          return reject(new EraMismatchError(queryEra, ledgerEra))
        } else {
          return reject(new UnknownResultError(response.result))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'Query',
        args: {
          query: {
            nonMyopicMemberRewards: input
          }
        },
        mirror: { requestId }
      } as Ogmios['Query']))
    })
  },
  context
  )
}
