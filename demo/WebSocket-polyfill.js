WebSocket.prototype.wsp = function wsp(methodname, args = {}, mirror = null) {
  this.send(JSON.stringify({
    type: "jsonwsp/request",
    version: "1.0",
    methodname,
    args,
    mirror
  }));
};
