//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

/*
 * Rapid integration for Ogmios. Expected to be run within the context of 'tests.html' which
 * makes Mocha & Chai available as global context, as well as a global constant: OGMIOS_URL
 * contained the websocket URL based on the browser location.
 *
 */

const BATCH_SIZE = 1000; // blocks
const AT_LEAST = 20000; // blocks
const TIMEOUT = 15; // seconds

const lastByronBlock = {
  slot: 4492799,
  hash: "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457"
};

const lastShelleyBlock = {
  slot: 16588702,
  hash: "2772d6484501a4183f3c2376536234c0a27e1da1ed282f9fd1b32890605ee241"
};

describe("ChainSync", () => {
  let client;

  beforeEach(done => {
    client = new WebSocket(OGMIOS_URL);
    done();
  });

  afterEach(done => {
    client.close();
    done();
  });

  testChainSync("Byron Era", [ "origin" ]);
  testChainSync("Shelley Era", [ lastByronBlock ]);
  testChainSync("Allegra Era", [ lastShelleyBlock ]);

  function nextBatch(N) {
    for (let n = 0; n < N; n += 1) {
      client.ogmios("RequestNext");
    }
  }

  function testChainSync(title, points) {
    it(title, function (done) {
      const test = this.test;
      this.timeout(TIMEOUT*1000);

      const start = Date.now();
      let size = 0;
      let rcvd = 0;

      client.onopen = function() {
        client.ogmios("FindIntersect", { points });
      };

      client.onmessage = function(event) {
        const response = JSON.parse(event.data);

        switch (response.methodname) {
          case "FindIntersect":
            nextBatch(BATCH_SIZE);
            break;

          default:
            rcvd += 1;
            size += event.data.length * 16; // 2 bytes / 16 bits per UTF-16 character

            if ((rcvd + BATCH_SIZE / 2) % BATCH_SIZE == 0) {
              if (rcvd < AT_LEAST) {
                nextBatch(BATCH_SIZE);
              } else {
                const time = Date.now() - start;
                const syncSpeed = 1000 * rcvd / time;
                const downSpeed = 1000 * (size / (1024*1024)) / time;
                const results = {
                  "block/s": syncSpeed.toFixed(0),
                  "mbits/s": downSpeed.toFixed(0),
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
