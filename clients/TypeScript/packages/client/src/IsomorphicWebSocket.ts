import IsoWebSocket from 'isomorphic-ws'

/**
 * @Internal
 */
export class WebSocket extends IsoWebSocket {
  constructor (addr : string | URL, opts? : IsoWebSocket.ClientOptions) {
    super(addr, opts)
    return isBrowser ? Object.assign(this, browserPolyfill(this as unknown as EventTarget)) : this
  }
}

/**
 * @Internal
 */
export interface CloseEvent {
  wasClean: boolean;
  code: number;
  reason: string;
  type: string;
  target: IsoWebSocket;
}

/**
 * @Internal
 */
type WebSocketEvent =
  | 'open'
  | 'upgrade'
  | 'message'
  | 'close'
  | 'error'

/**
 * @Internal
 */
const browserPolyfill = (target : EventTarget) : IsoWebSocket => {
  let listeners : [WebSocketEvent, EventListener, EventListener][] = []

  function removeListener (event : WebSocketEvent, fn : EventListener) {
    listeners = listeners.reduce((acc, [e, orig, handler]) => {
      if (e === event && orig === fn) {
        target.removeEventListener(e, handler)
      } else {
        acc.push([e, orig, handler])
      }
      return acc
    }, [])
  }

  return {
    on (event : WebSocketEvent, fn: EventListener) {
      const handler : EventListener = (e : MessageEvent) => typeof e.data !== 'undefined' ? fn(e.data) : fn(e)
      listeners.push([event, fn, handler])
      target.addEventListener(event, handler)
    },

    once (event : WebSocketEvent, fn : any) {
      const handler = (e : MessageEvent) => {
        removeListener(event, fn)
        typeof e.data !== 'undefined' ? fn(e.data) : fn(e)
      }
      listeners.push([event, fn, handler])
      target.addEventListener(event, handler, { once: true })
    },

    removeListener,

    removeAllListeners () {
      listeners.forEach(([event, _orig, handler]) =>
        target.removeEventListener(event, handler)
      )
      listeners = []
    }
  } as IsoWebSocket
}

/**
 * @Internal
 */
const isBrowser = typeof process === 'undefined'
