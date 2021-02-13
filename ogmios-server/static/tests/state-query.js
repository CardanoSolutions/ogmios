//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

/*
 * Rapid integration for Ogmios. Expected to be run within the context of 'tests.html' which
 * makes Mocha & Chai available as global context, as well as a global constant: OGMIOS_URL
 * contained the websocket URL based on the browser location.
 *
 */

describe("StateQuery", () => {
  let client;
  let listener;

  before(done => {
    client = new WebSocket(OGMIOS_URL);

    client.addEventListener('open', () => {
      client.ogmios("FindIntersect", { points: ["origin"] });
    });

    client.addEventListener('message', function listener(msg) {
      const response = JSON.parse(msg.data);
      switch (response.methodname) {
        case "FindIntersect":
          const point = response.result.IntersectionFound.tip;
          client.ogmios("Acquire", { point });
          break;

        default:
          client.removeEventListener('message', listener);
          done();
      }
    });
  });

  afterEach(() => {
    client.removeEventListener('message', listener);
  });

  const queries =
          [ [ "ledgerTip", { timeout: 2000 } ]
          , [ "currentEpoch", { timeout: 2000 }]
          , [ "currentProtocolParameters", { timeout: 2000 } ]
          , [ "proposedProtocolParameters", { timeout: 2000 } ]
          , [ "stakeDistribution", { timeout: 5000 } ]
          , [ { "utxo":
                [ "Ae2tdPwUPEYx54UZRbbU8M8HjXMRqoWXYwgpN3GUuzMQBUFRyrco4jBHZgd"
                , "addr1qy9y9s40l30283zec5svd6pky39qsae3mwg4an66f28zlns2gtp2llz750z9n3fqcm5rvfz2ppmnrku3tm845j5w9l8qdpmv0m"
                ]
              }
            , { "timeout": 5000 }
            ]
          ]

  const gibberish =
          [ "notAQuery"
          ]

  queries.forEach(([query, { timeout }]) => {
    const title = typeof query === "string" ? query : Object.keys(query).join("/");
    it(title, function () {
      const test = this.test;
      this.timeout(timeout);
      return new Promise ((resolve, reject) => {
        client.addEventListener('message', function $listener(msg) {
          listener = $listener;
          const response = JSON.parse(msg.data).result;
          expect(response.eraMismatch).to.be.undefined;
          test.result = JSON.stringify(response, null, 4);
          resolve();
        });

        client.ogmios("Query", { query });
      });
    });
  });

  gibberish.forEach((query) => {
    it(query, function () {
      const test = this.test;
      return new Promise ((resolve, reject) => {
        client.addEventListener('message', function $listener(msg) {
          listener = $listener;
          const response = JSON.parse(msg.data);
          expect(response.type).to.equal('jsonwsp/fault');
          expect(response.fault).to.have.property('code');
          expect(response.fault).to.have.property('string');
          test.result = JSON.stringify(response, null, 4);
          resolve();
        });

        client.ogmios("Query", { query });
      });
    });
  });

});
