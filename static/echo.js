importScripts("require.js");

var backlog = [];

var handler = function backlog_handler(e) {
  backlog.push(e);
};

onmessage = function(e) {
  return handler(e)
};

require([], function worker() {
  handler = function(e) {
    postMessage(e.data); // echo
  };
  postMessage("READY");
  for (var i in backlog) handler(backlog[i]);
  backlog = [];
});
