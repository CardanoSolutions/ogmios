import { nanoid } from 'nanoid'
import { Data } from 'isomorphic-ws'
import { ConnectionConfig, InteractionContext } from '../Connection'
import { baseRequest, send } from '../Request'

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
    config?: ConnectionConfig | InteractionContext
  ): Promise<Response> =>
    send<Response>((socket) =>
      new Promise((resolve, reject) => {
        const requestId = nanoid(5)

        async function listener (data: Data) {
          const queryResponse = JSON.parse(data as string) as QueryResponse
          if (queryResponse.reflection?.requestId !== requestId) { return }
          await response.handler(
            queryResponse,
            resolve,
            reject
          )
          socket.removeListener('message', listener)
        }

        socket.on('message', listener)
        socket.send(JSON.stringify({
          ...baseRequest,
          methodname: request.methodName,
          args: request.args,
          mirror: { requestId }
        } as unknown as Request))
      }), config)
