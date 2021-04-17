//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

/*
 * Rapid integration for Ogmios. Expected to be run within the context of 'tests.html' which
 * makes Mocha & Chai available as global context, as well as a global constant: OGMIOS_URL
 * contained the websocket URL based on the browser location.
 *
 */

const q = window.location.search
    .replace('?', '')
    .split('&')
    .map(x => x.split('='))
    .reduce((o, [k,v]) => Object.assign(o, { [k]: v }), {});

const N_MAX = Number(q['max'] || 10000); // blocks
const N_BOOTSTRAP = Number(q['bootstrap'] || 1000); // blocks
const TIMEOUT = Number(q['timeout'] || 15); // seconds

const lastByronBlock = {
  slot: 4492799,
  hash: "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457"
};

const lastShelleyBlock = {
  slot: 16588737,
  hash: "4e9bbbb67e3ae262133d94c3da5bffce7b1127fc436e7433b87668dba34c354a"
};

const lastAllegraBlock = {
  slot: 23068793,
  hash: "69c44ac1dda2ec74646e4223bc804d9126f719b1c245dadc2ad65e8de1b276d7"
};

[[], ["ogmios.v1:compact"]].forEach(subProtocols => {
  const suffix = subProtocols.length != 0 ? " "+subProtocols : "";
  describe("ChainSync"+suffix, () => {
    let client;

    beforeEach(done => {
      client = new WebSocket(OGMIOS_URL, subProtocols);
      done();
    });

    afterEach(done => {
      client.close();
      done();
    });

    testChainSync(client, "Byron Era"+suffix, []);
    testChainSync(client, "Shelley Era"+suffix, [ lastByronBlock ]);
    testChainSync(client, "Allegra Era"+suffix, [ lastShelleyBlock ]);
    testChainSync(client, "Mary Era"+suffix, [ lastAllegraBlock ]);

    function testChainSync(fixture, title, points) {
      it(title, function (done) {
        const test = this.test;
        this.timeout(TIMEOUT*1000);

        let start;
        let size = 0;
        let rcvd = 0;

        let didBootstrap = false;

        client.onopen = function() {
          client.ogmios("FindIntersect", { points });
        };

        client.onmessage = function(event) {
          const response = JSON.parse(event.data);

          switch (response.methodname) {
            case "FindIntersect":
              start = Date.now();
              client.ogmios("RequestNext");
              break;

            default:
              if (!didBootstrap) {
                didBootstrap = true;
                for(let n = 0; n <= N_BOOTSTRAP; n += 1) {
                  client.ogmios("RequestNext");
                }
              } else {
                rcvd += 1;
                size += event.data.length;

                if (rcvd < N_MAX - N_BOOTSTRAP) {
                  client.ogmios("RequestNext");
                }

                if (rcvd == N_MAX) {
                  const time = Date.now() - start;
                  const mb = size / (1024*1024);
                  const syncSpeed = 1000 * rcvd / time;
                  const downSpeed = 1000 * mb / time;
                  const results = {
                    "totalBlocks": rcvd,
                    "totalTime": (time / 1000).toFixed(3) + "s",
                    "totalSize": mb.toFixed(2) + "MB",
                    "block/s": syncSpeed.toFixed(0),
                    "MB/s": downSpeed.toFixed(0),
                    "lastBlock": response.result,
                  };
                  test.result = JSON.stringify(results, null, 2);
                  done();
                }
              }
          }
        };
      });
    }
  });
});
