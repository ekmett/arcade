define("connection",[], function() {

  var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;

  var backlog = [];

  var host = window.location.host;
  var port = window.location.port || 8080;

  if (host == '') host = 'localhost';

  var uri = 'ws://' + host + ':' + port + "/connect";

  var ws = null; // web socket

  var connection = {
    shutdown: false,
    available: false,
    uri: uri,
    reconnectable: true,
    attempts: 3,
    ticket: undefined,
    onmessage: function(e) { console.log("connection","onmessage","handler",e.data); }
  };

  var states = null; // assigned below
  var state = "DISCONNECTED";

  var open = function() {
    ws = new Socket(typeof connection.ticket == 'undefined' ? uri : uri + "/" + connection.ticket);
    state = typeof connection.ticket == 'undefined' ? "CONNECTING" : "CONNECTED";

    ws.onmessage = function(event) {
      return states[state].onmessage(event);
    };

    ws.onopen = function(event) {
      return states[state].onopen(event);
    };

    ws.onclose = function(event) {
      return states[state].onclose(event);
    };
    ws.onerror = function(event) {
      return states[state].onerror(event);
    }

  };

  var attempt = 0;

  var reconnect = function() {
    if (!connection.shutdown && connection.reconnectable && attempt++ < connection.attempts) {
      if (ws != null) ws.close();
      connection.available = false;
      ws = open(connection.ticket);
      state = typeof connection.ticket === 'undefined' ? "CONNECTING" : "CONNECTED"
      // TODO: add a connection timeout here and try reconnect again if the connection is stll unavailable then
    } else {
      ws = null;
      state = "SHUTDOWN";
    }
  };

  // created above
  states = {
    DISCONNECTED: {
      onmessage: function(e) {
        console.warn("connection","onmessage", "DISCONNECTED", e.data);
      },
      onclose: function() {
        console.warn("connection","onclose", "DISCONNECTED");
      },
      onerror : function() {
        console.warn("connection", "onerror", "DISCONNECTED");
      },
      send: function(e) {
        backlog.push(e);
      },
      onopen: function() {
        // if we have a ticket go to connected, otherwise connectng
        if (typeof connection.ticket === 'undefined') {
          state = "CONNECTING";
        } else {
          state = "CONNECTED";
        }
      }
    },
    CONNECTING: {
      onopen: function(e) {
        console.log("connection","onopen", "CONNECTING");
        // still waiting for ticket #
      },
      onmessage: function(e) {
        console.log("connection","onmessage", "CONNECTING", e.data);
        if ((connection.ticket || e.data) == e.data) {
          connection.ticket = e.data;
          attempt = 0; // we made it
          state = "CONNECTED";
          for (var i in backlog)
            ws.send(backlog[i]);
          connection.available = true;
          backlog = [];
        } else {
          console.error("connection","onmessage","ticket mismatch:",connection.ticket,e.data);
          state = "SHUTDOWN";
          connection.shutdown = true;
          connection.available = false;
          backlog = [];
          ws.close();
        }
      },
      onclose: function() {
        console.warn("connection","onclose", "CONNECTING");
        reconnect();
      },
      onerror: function(e) {
        console.warn("connection","onerror","CONNECTING");
        reconnect();
      },
      send: function(e) {
        console.warn("connection","send","CONNECTING","queuing:",e);
        backlog.push(e);
      }
    },
    CONNECTED: {
      onopen: function(e) {
        for (var i in backlog)
          ws.send(backlog[i]);
        backlog = [];
        connection.available = true;
        // still waiting for ticket #
      },
      onmessage: function(e) {
        console.log("connection","onmessage","CONNECTED",e.data);
        connection.onmessage(e);
      },
      onclose: function() {
        console.warn("connection","onclose", "CONNECTED");
        connection.available = false;
        reconnect();
      },
      onerror: function(e) {
        console.warn("connection","onerror","CONNECTED");
        connection.available = false;
        reconnect();
      },
      send: function(e) {
        if (connection.available) {
          console.warn("connection","send","CONNECTED","sending:",e);
          for (var i in backlog)
            ws.send(backlog[i]);
          backlog = [];
          ws.send(e);
        } else {
          console.warn("connection","send","CONNECTED","queuing:",e);
          backlog.push(e);
        }
      }
    },
    SHUTDOWN: {
      onmessage: function(e) {
        console.debug('connection', 'onmessage', "SHUTDOWN", e.data);
      },
      onclose: function(e) {
        console.debug('connection', 'onclose', "SHUTDOWN");
      },
      onerror: function(e) {
        console.debug('connection', 'onerror', "SHUTDOWN");
      },
      send: function(e) {
        console.warn("connection","send","SHUTDOWN","queuing forever",e);
        backlog.push(e);
      }
    },
  };

  connection.start = function() {
    connection.shutdown = false;
    if (state == 'DISCONNECTED' || state == "SHUTDOWN") {
      backlog = [];
      if (typeof connection.ticket === 'undefined') {
        state = "CONNECTING";
      } else {
        state = "CONNECTED";
      }
      ws = open();
    } else {
      reconnect();
    }
  };

  connection.end = function() {
    if (ws != null) {
      connection.available = false;
      ws.close();
    }
    connection.shutdown = true;
    state = "SHUTDOWN";
  }

  return connection;
});
