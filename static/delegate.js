define([], function() {

var delegate = {
  via : {
    postMessage: function (e,xs) {
      console.log("delegate.via.postMessage: unspecified");
    }
  },
  targets : {
    delegate : delegate
  }
};

// { to: "delegate", from: "foo", request: "require", module: "physics/world", ticket: 1 }
// { to: "foo", from: "delegate", request: "result", result: "OK", ticket: 1 }
delegate.ask = function(e) {
  switch (e.data.request) {
    case "require": require([e.data.module], function(mod) { delegate.targets[e.data.module] = module });
  }
};

// this makes it a bit easier to split up a message queue for a web worker or page
delegate.message = function delegate_message(e) {
  var d = e.data;
  var target = delegate.targets[d.to];
  if (target) {
    var result = target.ask(e);
    if (typeof e.data.ticket !== 'undefined') {
      delegate.via.postMessage({to: d.from, from: d.to, request: "result", ticket: d.ticket, result: result });
    }
  } else console.log("delegate.onmessage: Unknown recipient for message", e)
};

delegate.add = function delegate_add(name, target) {
  if (target.onmessage) targets[name] = target;
  else console.log("delegate.add: No onmessage handler for delegate");
};

return delegate;

});
