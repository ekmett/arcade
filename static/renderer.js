define(
  "renderer",
  ["jquery", "shim/raf", "stats", "events"],
  function($, raf, stats, events) {

    var main = $("#main");

    var Layer = function(name) {
      this.name = name;
      this.canvas = $("#" + name)[0];
      this.context = this.canvas.getContext("2d");
      this.dirty = null; // TODO: permit dirty rectangles vs. scrub and redraw vs. scrolling
    };

    Layer.prototype = {
      ball : function(x,y,z,r,c) {
        var context = this.context;
        context.beginPath();
        context.arc(x,y-z,r,0,2*Math.PI,false);
        context.fillStyle = c || "#3A5BCD";
        context.fill();
        // dirty the containing rectangle
      },
      clear: function() {
        // TODO: check dirty and scrub a rectangle if we can
        this.canvas.width = document.body.clientWidth;
      }
    };

    var background = new Layer("background");
    var shadows    = new Layer("shadows");
    var dynamic    = new Layer("dynamic");
    var hud        = new Layer("hud");

    var layers = [
      background,
      shadows,
      dynamic,
      hud
    ];

    var lastX = null;
    var lastY = null;

    var resized = function() {
      var w = document.body.clientWidth;
      var h = document.body.clientHeight;
      main[0].width  = w;
      main[0].height = h;
      for (var i in layers) {
        var canvas = layers[i].canvas;
        canvas.width = w;
        canvas.height = h;
      }
    };

    $(window).bind("resize", resized);
    $(window).ready(resized);

    var render = function(t) { // t is a DOMHighResTimeStamp or Date
      requestAnimationFrame(render);
      stats.begin();

      // demonstrate the benefits of static layers
      if (events.mouseX != lastX || events.mouseY != lastY) {
        lastX = events.mouseX;
        lastY = events.mouseY;

        dynamic.clear();
        dynamic.ball(events.mouseX, events.mouseY, 10, "#3A5BCD");

        // show random crap to slow down the canvas for testing
        for (var i = 0; i < 2000; i ++) {
          dynamic.ball(events.mouseX % i, i % events.mouseY, 10, 10, "#001020");
        }

        hud.clear();
        for (var i = 0; i < 200; i++) {
          hud.ball(i % events.mouseX, events.mouseY % (i * 10), 10, 10, "#802010");
        }
      }

      stats.end();
    };

    render(Date.now());
});
