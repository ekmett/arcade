importScripts("require.js");

var backlog = [];

var handler = function backlog_handler(e) {
  backlog.push(e);
};

// delegate to handler.
function onmessage(e) {
  return handler(e)
};

require(["delegate"], function () {
  // replace handler, now that the world is loaded and clear backlog
  handler = delegate.onmessage;
  delegate.via = self;
  for (var i in backlog) handler(backlog[i]);
  backlog = [];
  postMessage({to:"main", request: "start"});
});
