const IsoWebSocket = require('isomorphic-ws')

module.exports = class WebSocket extends IsoWebSocket {
  constructor(addr, opts) {
    super(addr, opts)
    return isBrowser ? Object.assign(this, browserPolyfill(this)) : this
  }
}

const browserPolyfill = (target) => {
  let listeners = []

  function removeListener(event, fn) {
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
    on(event, fn) {
      const handler = ({ data }) => fn(data)
      listeners.push([event, fn, handler])
      target.addEventListener(event, handler)
    },

    once(event, fn) {
      const handler = ({ data }) => {
        removeListener(event, fn)
        fn(data)
      }
      listeners.push([event, fn, handler])
      target.addEventListener(event, handler, { once: true })
    },

    removeListener,

    removeAllListeners() {
      listeners.forEach(([event, _orig, handler]) =>
        target.removeEventListener(event, handler)
      )
      listeners = []
    }
  }
}

/**
 * @Internal
 */
const isBrowser = typeof process === 'undefined'
