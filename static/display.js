define(
  ["jquery", "physics", "shim/raf", "shim/cc", "performance", "stats", "prim", "transformations","events","toggles", "animation" ],
  function display($, physics, raf, cc, performance, stats, prim, transformations,events, toggles, animation) {

var display = {
  cursor : new transformations.WorldPoint(),
  camera : null
};

var updated = 0;
var frame = 0;

var PIXELS_PER_METER = 16;
var METERS_PER_PIXEL = 1 / PIXELS_PER_METER;
var RECIP_Y_SCALE    = 2;
var Y_SCALE    = 0.5;
var halfWidth  = 400;
var halfHeight = 200;
var width      = 800; // set during resize
var height     = 400; // set during resize

var bumper = 4.3; // number of meters to adjust for display
var minA = -10 - bumper, minB = -10 + bumper;
var maxA = 10 - bumper, maxB = 10 - bumper;

// -14.1 0 top
// -0.1 5.2 left
// 5.7 -025 bottom
// 0.1 -5.448 right


/*
$('#glass').bind('mousewheel', function(e){
   if(e.originalEvent.wheelDelta > 0) {
       PIXELS_PER_METER /= 1.1;
       METERS_PER_PIXEL *= 1.1;
   }else {
       PIXELS_PER_METER *= 1.1;
       METERS_PER_PIXEL /= 1.1;
   }
   //prevent page fom scrolling
   return false;
 });
*/

var layer = function layer(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  result.canvas.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

var layers = [ background, shadows, foreground ];

var draw_background = function() {
  var b = background.canvas;
  b.clear(true);
  b.lineWidth = 0.3;

  // draw the world
  prim.room(b,-physics.SCENE_WIDTH/2,-physics.SCENE_DEPTH/2,0,physics.SCENE_WIDTH,physics.SCENE_DEPTH,physics.SCENE_HEIGHT);
  b.strokeStyle = "#ccc";
  b.fillStyle = "#ddd";
  b.fill();
  b.stroke();

  prim.floor(b,-physics.SCENE_WIDTH/2,-physics.SCENE_DEPTH/2,0,physics.SCENE_WIDTH,physics.SCENE_DEPTH,physics.SCENE_HEIGHT);
  b.fillStyle = "#fff";
  b.fill();
};

var render = function render() {
  var t = performance.now();
  var pt = physics.updated;

  requestAnimationFrame(render);

  // no physics yet
  if (!pt) return;

  var alpha = (t - pt) / physics.MILLISECONDS_PER_FRAME;

  stats.display.begin();

  toggles.bounding = events.impulse[66];
  toggles.soft_shadows = events.impulse[67];

  // move to events?
  display.cursor.screen(
    (events.mouseX-halfWidth) * METERS_PER_PIXEL,
    (events.mouseY-halfHeight) * METERS_PER_PIXEL
  );


  var s = shadows.canvas;
  s.clear(true);

  var c = foreground.canvas;
  c.clear(true);

  for (var i in physics.particles) {
    var b = physics.particles[i];
    b.interpolate(alpha);
  }

  if (display.camera) {
    var x = display.camera.rx;
    var y = display.camera.ry;
    var a = Math.max(minA, Math.min(maxA, (x + y)/Math.sqrt(2)));
    var b = Math.max(minB, Math.min(maxB, (x - y)/Math.sqrt(2)));
    transformations.scrollX = (a+b)/Math.sqrt(2);
    transformations.scrollY = (a-b)/Math.sqrt(2);
  }
  draw_background();

  physics.particles.sort(function(a,b) {
    return a.key - b.key;
  });

  for (var i in physics.particles) {
    physics.particles[i].draw(s,c,alpha);
  }

  stats.display.end();
}


/*
$(window).bind("resize", resized);
*/

$(window).ready(function() {
  draw_background();
  render(performance.now());
});

// this may have been a bad idea
Object.defineProperties(display, {
  /* constants */
  // PIXELS_PER_METER : { value: PIXELS_PER_METER, __proto__ : null, configurable: false, writable: false, enumerable: true },
  // METERS_PER_PIXEL : { value: METERS_PER_PIXEL, __proto__ : null, configurable: false, writable: false, enumerable: true },
  PIXELS_PER_METER : { get: function() { return PIXELS_PER_METER },   __proto__ : null, configurable: false, enumerable: true },
  METERS_PER_PIXEL : { get: function() { return METERS_PER_PIXEL }, __proto__ : null, configurable: false, enumerable: true },
  /* read-only attributes */
  frame      : { get: function() { return frame },   __proto__ : null, configurable: false, enumerable: true },
  updated    : { get: function() { return updated }, __proto__ : null, configurable: false, enumerable: true },
  halfHeight : { get: function() { return halfHeight },   __proto__ : null, configurable: false, enumerable: true },
  halfWidth  : { get: function() { return halfWidth }, __proto__ : null, configurable: false, enumerable: true },
  height     : { get: function() { return height },   __proto__ : null, configurable: false, enumerable: true },
  width      : { get: function() { return width }, __proto__ : null, configurable: false, enumerable: true },
  foreground : { get: function() { return foreground }, __proto__ : null, configurable: false, enumerable: true },
});

return display;

});
