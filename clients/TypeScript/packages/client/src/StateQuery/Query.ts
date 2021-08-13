import { nanoid } from 'nanoid'
import { InteractionContext } from '../Connection'
import { baseRequest, send } from '../Request'
import { safeJSON } from '../util'

/** @internal */
export const Query = <
  Request,
  QueryResponse extends { reflection?: { requestId?: string } },
  Response
  >(
    request: {
    methodName: string,
    args?: any
  },
    response: {
    handler: (
      response: QueryResponse,
      resolve: (value?: Response | PromiseLike<Response>) => void,
      reject: (reason?: any) => void
    ) => void
  },
    context: InteractionContext
  ): Promise<Response> =>
    send<Response>((socket) =>
      new Promise((resolve, reject) => {
        const requestId = nanoid(5)

        async function listener (data: string) {
          const queryResponse = safeJSON.parse(data) as QueryResponse
          if (queryResponse.reflection?.requestId !== requestId) { return }
          await response.handler(
            queryResponse,
            resolve,
            reject
          )
          socket.removeListener('message', listener)
        }

        socket.on('message', listener)
        socket.send(safeJSON.stringify({
          ...baseRequest,
          methodname: request.methodName,
          args: request.args,
          mirror: { requestId }
        } as unknown as Request))
      }), context)
