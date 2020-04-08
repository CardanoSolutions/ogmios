const socket = new WebSocket("wss://ogmios.dev");

socket.onopen = function (event) {
    socket.wsp("FindIntersect", { points: ["origin"] }, { step: "INIT" });
};

socket.onmessage = function (event) {
  const msg = JSON.parse(event.data);

  switch (msg.reflection.step) {

    /* As a first step, we ask the node with an intersection at the origin
     * point. This will _necessarily_ succeeds, initialize a client state
     * with a cursor pointing to genesis and returns the node current tip.
     *
     * From here, we could now call 4 million times 'RequestNext' to get
     * all blocks from the blockchain since genesis. Or... we can ask for
     * a new intersection and jump directly to the tip!
     */
    case "INIT":
      const { slot, hash } = msg.result.IntersectionFound.tip
      socket.wsp("FindIntersect", { points: [ { slot, hash } ] }, { step: "JUMP" });
    break;

    /* This requests is more delicate and subject to failure (although
     * very unlikely to happen on the federated Byron network). Between
     * the moment we received the previous response and the moment we
     * submitted the tip for an intersection, it is _possible_ that the node
     * switched to a different fork and no longer knows the given tip!
     *
     * In such case, we simply retry ... and retry. Ad-infinitum. In practice,
     * this won't happen very often (a.k.a never).
     *
     * Otherwise, we can start following the chain by requesting the next
     * block from the node.
     */
    case "JUMP":
      if (msg.result.IntersectionNotFound) {
        const { slot, hash } = msg.result.IntersectionNotFound.tip
        socket.wsp("FindIntersect", { points: [ { slot, hash } ] }, { step: "JUMP" });
      } else {
        socket.wsp("RequestNext", {}, { step: "NEXT" });
      }
    break;

    /* Following the chain in this scenario is super simple. Since we only care
     * about the last block (a.k.a the tip) which is given for both RollForward
     * and RollBackward response type, the handling is very simple here.
     *
     * In a more "real" application, we would want to keep an history of at
     * least `k=2160` last blocks to allow rewinding to anywhere in this history.
     */
    case "NEXT":
      console.log(msg.result);

      const tip = msg.result.RollForward
        ? msg.result.RollForward.tip
        : msg.result.RollBackward.tip;

      dispatch(tip);

      socket.wsp("RequestNext", {}, { step: "NEXT" });
    break;
  }
};

socket.onclose = function (event) {
  console.log("Connection closed.");
};

/* We don't want to 'pollute' the demo here with whatever is done
 * with the data we receive from the network. So, we simply dispatch
 * an event with some data in it.
 *
 * On the other side, we can simply do:
 *
 *     window.addEventListener('block', block => ...)
 *
 * to do anything with the data.
 */
function dispatch(ev) {
  window.dispatchEvent(new CustomEvent('block', { detail: ev }));
}
