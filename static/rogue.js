(function() {
  requirejs.config({
    paths: {
      'bootstrap'            : 'http://netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min',
      'jquery'               : 'http://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min',
      'jquery.fullscreen'    : 'jquery.fullscreen.min'
    },
    shim: {
      "rogue" : {
      },
      "bootstrap": {
        deps: ["jquery"],
        exports: "$.fn.popover"
      },
      "jquery.fullscreen": {
        deps: ["jquery"],
        exports: "$.fn.fullScreen"
      }
    },
    enforceDefine: true
  });
  define(
    ['jquery','bootstrap','connection','chat','stats','music','physics','display'],
    function ($, bootstrap, connection, chat, stats, music, physics, display) {
      $(document).ready(function() {
        $("body").append(stats.domElement);
         // music.play();

/*
        connection.onmessage = function(event) {
          console.log("MESSAGE:",event.data);
          eval(event.data);
        };
        connection.start();
*/
      });
    }
  );
})();
