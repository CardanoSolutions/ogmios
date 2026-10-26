import { nanoid } from 'nanoid'
import { CustomError } from 'ts-custom-error'
import { WebSocket, CloseEvent } from './IsomorphicWebSocket'
import { getServerHealth, ServerNotReady } from './ServerHealth'
import { safeJSON } from './util'

/**
 * Connection configuration parameters. Use `tls: true` to create a `wss://` using TLS
 * encryption (if supported by the server).
 *
 * @category Connection
 */
export interface ConnectionConfig {
  address?: {
    http: string,
    webSocket: string,
  },
  host?: string,
  port?: number,
  tls?: boolean,
  maxPayload?: number
}

/** @category Connection */
export interface Connection {
  maxPayload: number,
  address: {
    http: string
    webSocket: string
  }
}

/**
 * An interaction context used by Ouroboros clients to interact with the server.
 *
 * @category Connection
 */
export interface InteractionContext {
  connection: Connection
  socket: WebSocket
}

/**
 * Describe how the interaction context behaves. A `LongRunning` context does not close
 * the underlying connection after a request, it has to be done manually. A `OneTime` context
 * however will close the connection afterwards.
 *
 * @category Connection
 */
export type InteractionType = (
  | 'LongRunning'
  | 'OneTime'
)

/**
 * A value set when sending the request, to be mirrored by the server in the corresponding response.
 *
 * @category Connection
 */
export type Mirror = { [k: string]: unknown }

/** @category Connection */
export type WebSocketErrorHandler = (error: Error) => void

/** @category Connection */
export type WebSocketCloseHandler = (
  code: CloseEvent['code'],
  reason: CloseEvent['reason']
) => void

/** @category Connection */
export class JSONRPCError extends Error {
  code: number
  data?: any
  id?: any

  public constructor (code: number, message: string, data?: any, id?: any) {
    super(message)
    this.stack = ''
    this.code = code

    if (typeof data !== 'undefined') { this.data = data }
    if (typeof id !== 'undefined') { this.id = Object.assign({}, id || {}) }
  }

  public static tryFrom (any: any) {
    if ('error' in any && 'jsonrpc' in any && any.jsonrpc === '2.0') {
      const { error: e } = any
      if ('code' in e && 'message' in e) {
        if (Number.isInteger(e.code) && typeof e.message === 'string') {
          return new JSONRPCError(e.code, e.message, e?.data, any?.id)
        }
      }
    }

    return null
  }
}

/**
 * Raised when a request made with a {@link MethodOptions} `timeout` receives no response in time.
 *
 * @category Connection
 */
export class RequestTimeoutError extends CustomError {
  public readonly timeout: number

  public constructor (timeout: number) {
    super(`Request timed out after ${timeout}ms`)
    this.timeout = timeout
  }
}

/** @category Constructor */
export function createConnectionObject (config?: ConnectionConfig): Connection {
  if (config?.address && (config?.host || config?.port || config?.tls)) {
    throw new Error('invalid connection configuration: cannot contain both address AND host/port/tls')
  }

  const _128MB = 128 * 1024 * 1024

  const base = {
    host: config?.host ?? '127.0.0.1',
    port: config?.port ?? 1337,
    tls: config?.tls ?? false
  }

  const hostAndPort = `${base.host}:${base.port}`

  const defaultAddress = {
    http: `${base.tls ? 'https' : 'http'}://${hostAndPort}`,
    webSocket: `${base.tls ? 'wss' : 'ws'}://${hostAndPort}`
  }

  return {
    maxPayload: config?.maxPayload ?? _128MB,
    address: config?.address ?? defaultAddress
  }
}

/** @internal */
const DEFAULT_HEARTBEAT_INTERVAL = 30_000

/**
 * Starts a `ws`-level ping/pong heartbeat on an open socket: after `interval` ms of silence,
 * pings and waits for a reply; if none arrives (no pong, no other message) before the next tick,
 * terminates the connection (which fires `close`, letting the rest of the stack handle it as any
 * other disconnect). Any inbound traffic — a pong, or an ordinary message — counts as proof of
 * life and pushes the next check back by a full `interval`, so an actively-used connection never
 * gets pinged at all.
 *
 * @internal
 */
const startHeartbeat = (socket: WebSocket, interval: number) => {
  if (!interval || interval <= 0) { return }
  // A no-op when the socket doesn't support `ping` (e.g. a browser WebSocket, which exposes no such API)
  if (typeof socket.ping !== 'function') { return }

  let timer: ReturnType<typeof setTimeout>

  // Quiet for `interval` -> send a ping, then wait `interval` for any sign of life
  const probe = () => {
    if (socket.readyState !== socket.OPEN) { return }
    try { socket.ping() } catch { return socket.terminate() }
    timer = setTimeout(() => socket.terminate(), interval)
    timer.unref?.()
  }

  // Any activity: cancel whatever is pending and restart the idle countdown
  const onAlive = () => {
    clearTimeout(timer)
    timer = setTimeout(probe, interval)
    timer.unref?.()
  }

  socket.on('pong', onAlive)
  socket.on('message', onAlive)
  onAlive()

  socket.once('close', () => {
    clearTimeout(timer)
    socket.removeListener('pong', onAlive)
    socket.removeListener('message', onAlive)
  })
}

/** @category Constructor */
export const createInteractionContext = async (
  errorHandler: WebSocketErrorHandler,
  closeHandler: WebSocketCloseHandler,
  options?: {
    connection?: ConnectionConfig
    maxEventListeners?: number,
    /**
     * Ping interval, in milliseconds, used to detect a half-open connection (NAT timeout,
     * laptop sleep, dropped network) that would otherwise hang requests silently. Defaults to
     * 30s; pass `0` (or any non-positive value) to disable. Node-only: a no-op in environments
     * without access to WebSocket ping/pong frames (e.g. browsers).
     */
    heartbeat?: number
  }): Promise<InteractionContext> => {
  const { heartbeat = DEFAULT_HEARTBEAT_INTERVAL } = options ?? {}
  const connection = createConnectionObject(options?.connection)
  const health = await getServerHealth({ connection })
  return new Promise((resolve, reject) => {
    if (health.lastTipUpdate === null) {
      return reject(new ServerNotReady(health))
    }
    const socket = new WebSocket(connection.address.webSocket, { maxPayload: connection.maxPayload })

    const onInitialError = (error: Error) => {
      socket.removeAllListeners()
      return reject(error)
    }
    const onInitialClose = (_code: number, reason: string) => {
      socket.removeAllListeners()
      reject(new Error(reason))
    }
    socket.setMaxListeners(options?.maxEventListeners ?? 10)
    socket.on('error', onInitialError)
    socket.once('close', onInitialClose)
    socket.on('open', async () => {
      socket.removeListener('error', onInitialError)
      socket.removeListener('close', onInitialClose)
      socket.on('error', errorHandler)
      socket.on('close', closeHandler)

      startHeartbeat(socket, heartbeat)

      resolve({
        connection,
        socket
      })
    })
  })
}

/** @internal */
export const baseRequest = {
  jsonrpc: '2.0'
}

/** @internal */
export const ensureSocketIsOpen = (socket: WebSocket) => {
  if (socket.readyState !== socket.OPEN) {
    throw new Error('WebSocket is closed')
  }
}

/**
 * Options for the websocket send query awaiting response.
 *
 * @category Connection
 */
export type MethodOptions = {
  /**
   * Reject the call with a {@link RequestTimeoutError} if no response is received within this
   * many milliseconds. Unset by default (the call waits indefinitely). The server still
   * processes a timed-out request, so discard the connection afterwards; see "Request
   * Timeouts" in the README.
   */
  timeout?: number
}

/** @internal */
export const send = async <T>(
  send: (socket: WebSocket, onSettle: (cleanup: () => void) => void) => Promise<T>,
  context: InteractionContext,
  options?: MethodOptions
): Promise<T> => {
  const { socket } = context
  return new Promise((resolve, reject) => {
    let settled = false
    let cleanup: (() => void) | undefined
    let timer: ReturnType<typeof setTimeout> | undefined

    /**
     * Runs the full teardown and guards against running it multiple times.
     */
    function settleOnce (fn: () => void) {
      if (settled) { return }
      settled = true
      cleanup?.()
      if (timer !== undefined) { clearTimeout(timer) }
      socket.removeListener('close', onUnexpectedClose)
      fn()
    }

    function onUnexpectedClose (code: CloseEvent['code'], reason: CloseEvent['reason']) {
      settleOnce(() => reject(new JSONRPCError(
        -32000,
        'Connection closed',
        { code, reason }
      )))
    }

    socket.once('close', onUnexpectedClose)

    if (options?.timeout !== undefined) {
      timer = setTimeout(() => {
        settleOnce(() => reject(new RequestTimeoutError(options.timeout)))
      }, options.timeout)
    }

    send(socket, fn => { cleanup = fn })
      .then(result => settleOnce(() => resolve(result)))
      .catch(error => settleOnce(() => reject(JSONRPCError.tryFrom(error) || error)))
  })
}

/**
 * Fire a request and resolve once it's answered (response, failure, timeout, etc.).
 *
 * @internal
 */
export const Method = <
  Request extends { method: string, params?: any },
  Response extends { method: string, id?: { requestId?: string } },
  A>
  (
    req: {
      method: Request['method'],
      params?: Request['params'],
    },
    res: {
      handler?: (
        response: Response,
        resolve: (value: A | PromiseLike<A>) => void,
        reject: (reason?: any) => void
      ) => void
    },
    context: InteractionContext,
    options?: MethodOptions
  ): Promise<A> =>
    send<A>((socket, onSettle) =>
      new Promise((resolve, reject) => {
        const requestId = nanoid(16)

        async function listener (data: string) {
          const response = safeJSON.parse(data) as Response
          if (response?.id?.requestId !== requestId) { return }
          try {
            const handler = res.handler || ((response, resolve, reject) => {
              if (response.method === req.method && response) {
                const success = response as unknown as { result: A | PromiseLike<A> }
                resolve(success.result)
              } else {
                reject(response)
              }
            })
            await handler(
              response,
              resolve,
              reject
            )
          } catch (e) {
            return reject(e)
          }
        }

        ensureSocketIsOpen(socket)

        socket.on('message', listener)
        onSettle(() => socket.removeListener('message', listener))

        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: req.method,
          params: req.params,
          id: { requestId }
        } as unknown as Request))
      }), context, options)
