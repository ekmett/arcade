define(
  ["jquery", "physics", "shim/raf", "shim/cc", "performance", "stats", "prim"],
  function display($, physics, raf, cc, performance, stats, prim) {

var display = {
};

var updated = 0;
var frame = 0;

var PIXELS_PER_METER = 20;
var METERS_PER_PIXEL = 1 / PIXELS_PER_METER;
var RECIP_Y_SCALE    = 2;
var Y_SCALE    = 0.5;
var halfWidth  = 400;
var halfHeight = 200;
var width      = 800; // set during resize
var height     = 400; // set during resize
var scrollX    = 0;
var scrollY    = 0;

var layer = function layer(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

var layers = [ background, shadows, foreground ];

var resized = function resized() {
  width  = Math.min($(window).width() - foreground.offset().left, 800);
  height = Math.min($(window).height() - foreground.offset().top, 400);
  halfWidth = width / 2;
  halfHeight = height / 2;
  $(".playarea").each(function(i,e) {
    $(e).width(width);
    $(e).height(height);
  });
  console.log("play area resized to",width,height);
};

var draw_background = function() {
  var b = background.canvas;
  b.clear(true);
  b.setTransform(display.PIXELS_PER_METER,0,0,display.PIXELS_PER_METER,display.halfWidth,display.halfHeight);
  b.lineWidth = 0.3;

  // draw the world
  prim.room(b,-5,-5,0,10,10,2);
  b.strokeStyle = "#ccc";
  b.fillStyle = "#ddd";
  b.fill();
  b.stroke();

  prim.floor(b,-5,-5,0,10,10,2);
  b.fillStyle = "#fff";
  b.fill();
};


$(window).bind("resize", resized);
$(window).ready(resized);

var render = function render() {
  var t = performance.now();
  var pt = physics.updated;

  requestAnimationFrame(render);

  // no physics yet
  if (!pt) return;

  var alpha = (t - pt) / physics.MILLISECONDS_PER_FRAME;

  stats.display.begin();

  draw_background();

  var s = shadows.canvas;
  s.clear(true);
  s.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

  var c = foreground.canvas;
  c.clear(true);
  c.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

  for (var i in physics.particles) {
    var b = physics.particles[i];
    b.interpolate(alpha);
  }

  physics.particles.sort(function(a,b) {
    return a.key - b.key;
  });

  for (var i in physics.particles) {
    var b = physics.particles[i];
    b.draw(s,c);
  }

  stats.display.end();
}

draw_background();

render(performance.now());

// this may have been a bad idea
Object.defineProperties(display, {
  /* constants */
  PIXELS_PER_METER : { value: PIXELS_PER_METER, __proto__ : null, configurable: false, writable: false, enumerable: true },
  METERS_PER_PIXEL : { value: METERS_PER_PIXEL, __proto__ : null, configurable: false, writable: false, enumerable: true },
  /* read-only attributes */
  frame      : { get: function() { return frame },   __proto__ : null, configurable: false, enumerable: true },
  updated    : { get: function() { return updated }, __proto__ : null, configurable: false, enumerable: true },
  halfHeight : { get: function() { return halfHeight },   __proto__ : null, configurable: false, enumerable: true },
  halfWidth  : { get: function() { return halfWidth }, __proto__ : null, configurable: false, enumerable: true },
  height     : { get: function() { return height },   __proto__ : null, configurable: false, enumerable: true },
  width      : { get: function() { return width }, __proto__ : null, configurable: false, enumerable: true },
  /* read-write attributes */
  scrollX : { get: function() { return scrollX },   set: function(e) { scrollX = e },   __proto__ : null, configurable: false, enumerable: true },
  scrollY : { get: function() { return scrollY },   set: function(e) { scrollY = e },   __proto__ : null, configurable: false, enumerable: true },
});

return display;

});
