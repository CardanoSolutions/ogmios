window.addEventListener('load', function () {
  const WEBSOCKET_PROTOCOL = window.location.protocol === "http:" ? "ws" : "wss";
  const WEBSOCKET_URL = WEBSOCKET_PROTOCOL + "://" + window.location.host;
  const HEALTH_URL = window.location.protocol + "//" + window.location.host + '/health';

  // Augment the 'WebSocket' prototype for convenient use with Ogmios.
  WebSocket.prototype.ogmios = function ogmios(method, params, id = {}) {
    this.send(JSON.stringify({
      jsonrpc: "2.0",
      method,
      params,
      id
    }));
  };

  dispatch('websocket_url_change', { url: WEBSOCKET_URL });

  /* --------------------------------------------------------------------------
   * Setup
   * ------------------------------------------------------------------------ */

  const EPOCH_LENGTH = 432000; // TODO: Get this from the 'GetGenesisConfig' query.
  let eraStart;
  let tickerId;

  function createClient(url) {
    const client = new WebSocket(url);

    client.addEventListener('open', async () => {
      dispatch("current_network", { network: await getCurrentNetwork(client) });
      eraStart = await getEraStart(client);
      monitorHealth(client);
    });

    return client;
  }

  // Re-connect when disconnecting.
  let client = createClient(WEBSOCKET_URL);
  setInterval(() => {
    if(client.readyState == WebSocket.CLOSING || client.readyState == WebSocket.CLOSED) {
      dispatch('health_error');
      if (tickerId !== undefined) { clearInterval(tickerId); }
      client = createClient(WEBSOCKET_URL);
    }
  }, 5000)

  /* --------------------------------------------------------------------------
   * Initialization
   * ------------------------------------------------------------------------ */

  async function getCurrentNetwork(client) {
    return new Promise(resolve => {
      client.addEventListener('message', function $eraStart({ data }) {
        client.removeEventListener('message', $eraStart);
        const result = JSON.parse(data).result || {};
        switch (result.networkMagic) {
          case 764824073:
            return resolve("mainnet");
          case 1097911063:
            return resolve("testnet");
          case 1:
            return resolve("preprod");
          case 2:
            return resolve("preview");
          case 9:
            return resolve("vasil-dev");
          default:
            return resolve("unknown");
        }
      });
      client.ogmios('Query', { query: "genesisConfig" });
    });
  }

  async function getEraStart(client) {
    return new Promise(resolve => {
      client.addEventListener('message', function $eraStart({ data }) {
        client.removeEventListener('message', $eraStart);
        resolve(JSON.parse(data).result);
      });
      client.ogmios('Query', { query: "eraStart" });
    });
  }

  /* --------------------------------------------------------------------------
   * Monitoring
   * ------------------------------------------------------------------------ */

  /* Poll Ogmios '/health' and dispatch events on results.
   */
  function monitorHealth(client) {
    const state = { tip: {}, epoch: null };

    client.addEventListener('message', ({ data }) => {
      const response = JSON.parse(data);
      switch (response.reflection) {
        case "monitorHealth.currentEpoch":
          if (response.result != state.epoch) {
            dispatch('epoch_change', { epoch: response.result });
          }
          state.epoch = response.result;
          break;

        default:
          break;
      }
    });

    function tick() {
      fetch(HEALTH_URL)
        .then(response => response.json())
        .then(({ lastKnownTip, lastTipUpdate, networkSynchronization, metrics }) => {
          dispatch('health_tick', { metrics });

          if (state.tip.hash != lastKnownTip.hash) {
            state.tip = lastKnownTip;

            client.ogmios("Query", { query: "currentEpoch" }, "monitorHealth.currentEpoch");

            dispatch('tip_change', {
                epochLength: EPOCH_LENGTH,
                slotInEpoch: (lastKnownTip.slot - eraStart.slot) % EPOCH_LENGTH,
                hash: lastKnownTip.hash,
                lastKnownTip,
                lastTipUpdate,
                networkSynchronization,
            });
          };
        })
        .catch(() => dispatch('health_error'));
    }

    tick();
    tickerId = setInterval(tick, 2500);
  };

  /* --------------------------------------------------------------------------
   * Utils
   * ------------------------------------------------------------------------ */

  /* Broadcast an event to all possible listeners.
   */
  function dispatch(event, detail = {}) {
    window.dispatchEvent(new CustomEvent(event, { detail }));
  }
});
