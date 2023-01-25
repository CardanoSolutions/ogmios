const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function wsp(methodname, args = {}, mirror) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args,
        mirror
    }));
}

client.on('message', function(msg) {
    const response = JSON.parse(msg);

    if (!response.result.RollForward) {
      return wsp("RequestNext");
    }

    const block = response.result.RollForward.block;
    const era = Object.keys(block)[0];

    if (era === "byron") {
      return wsp("RequestNext");
    }

    block[era]
      .body
      .forEach((tx, ix) => {
        if (tx.inputSource == 'collaterals' && tx.body.collateralReturn != null) {
          const tx_ix = ('0000' + ix.toString(16)).slice(-4);
          const out_ix = ('0000' + tx.body.outputs.length.toString(16)).slice(-4);
          console.log(`INSERT INTO inputs(ext_output_reference, address, value, datum_info, script_hash, created_at, spent_at) SELECT x'${tx.id}${out_ix}${tx_ix}', address, value, datum_info, script_hash, created_at, spent_at FROM inputs WHERE output_reference = x'${tx.id}0000';`);
          console.log(`DELETE FROM inputs WHERE output_reference = x'${tx.id}0000';`);
          console.log("");
        }
      });

    const tip = response.result.RollForward.tip.blockNo;
    const current = block[era].header.blockHeight;

    if (tip - current < 100) {
      console.error(`${block[era].header.slot}.${block[era].headerHash}`);
      client.close();
    } else {
      wsp("RequestNext");
    }
});

client.once('open', () => {
    wsp("FindIntersect", { points: [ { "slot": 72316796, "hash": "c58a24ba8203e7629422a24d9dc68ce2ed495420bf40d9dab124373655161a20" } ] });
    for (let i = 0; i < 100; i += 1) {
      wsp("RequestNext");
    }
});
