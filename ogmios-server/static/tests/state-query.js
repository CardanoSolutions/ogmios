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
          [ [ "ledgerTip", { timeout: 500 } ]
          , [ "currentEpoch", { timeout: 500 }]
          , [ "currentProtocolParameters", { timeout: 500 } ]
          , [ "proposedProtocolParameters", { timeout: 500 } ]
          , [ "stakeDistribution", { timeout: 5000 } ]
          ]

  queries.forEach(([query, { timeout }]) => {
    it(query, function () {
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
      })
    });
  });
});
