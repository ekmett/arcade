requirejs.config({
  paths: {
    'bootstrap'            : 'http://netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min',
    'jquery'               : 'http://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min',
  },
  shim: {
    'bootstrap': {
      deps: ['jquery'],
      exports: '$.fn.popover'
    }
  },
  enforceDefine: true
});

define(
  ['jquery','bootstrap','connection','chat','stats','music','physics','player','spawn','display'],
  function ($, bootstrap, connection, chat, stats, music, physics, player, spawn, display) {
    function genocide() {
      physics.particles = [player];
      physics.constraints = [];
    }
    $(document).ready(function() {
      $('body').append(stats.domElement);
      $('.genocide-link').click(genocide);
      $(document.body).keydown(function(e) { if (e.which == 48) genocide(); });
      var echo = new Worker("echo.js");
      echo.onmessage = function(e) {
        console.log(e);
      };
      echo.postMessage("HELLO");
      // music.play();
      // connection.onmessage = function(event) { eval(event.data); }; connection.start();
    });

    function updateOverview() {
      $("#particle-count").text(physics.particles.length);
      $("#constraint-count").text(physics.constraints.length);
    }

    window.setInterval(updateOverview, 500);
  }
);
