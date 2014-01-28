function createWebSocket(path) {
  var host = window.location.hostname;
  if(host == '') host = 'localhost';
  var uri = 'ws://' + host + ':8080' + path;
  var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
  return new Socket(uri);
}

$(document).ready(function () {
  var ws = createWebSocket('/');
  ws.onopen = function() {
    ws.send('hi');
  };

  ws.onmessage = function(event) {
    eval(event.data);
  };
});
