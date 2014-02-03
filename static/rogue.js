(function() {
  requirejs.config({
    paths: {
      // 'jquery': 'https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min',
      'jquery'               : 'jquery-1.11.0.min',
      'jquery.fullscreen'    : 'jquery.fullscreen.min',
    }
  });
  require(
    ['jquery', 'connection', 'stats', 'music', 'physics', 'renderer'],
    function ($, connection, stats, music, physics, renderer) {

      $(document).ready(function() {
        $("body").append(stats.domElement);
        // music.play();

        connection.onmessage = function(event) {
          console.log("MESSAGE:",event.data);
          eval(event.data);
        };
        connection.start();
      });
    }
  );
})();
