define(["jquery", "physics", "shim/raf", "shim/cc", "performance", "stats", "events","images", "aabb"], function($, physics, raf, cc, performance, stats, events, images, aabb) {

var display = {
  updated : null,
  frame : 0
};

var main = $("#main");

var DIRTY_NOTHING    = 0;
var DIRTY_BACKGROUND = 1;
var DIRTY_SHADOWS    = 2;
var DIRTY_FOREGROUND = 4;
var DIRTY_EVERYTHING = 7;

var bufferX = 128;// padding we maintain for smooth scrolling purposes, initially split
var bufferY = 128; // padding we maintain for smooth scrolling purposes, initially split
var drawnX  = 0;   // x coordinate of the leftmost drawn pixel, not necessarily visible
var drawnY  = 0;   // y coordinate at the rightmost drawn pixel, not necessarily visible
var scrollX = 128; // set during renderAnimationFrame
var scrollY = 128; // set during renderAnimationFrame
var width   = 640; // set during resize
var height  = 480; // set during resize
var dirty   = DIRTY_EVERYTHING;

var yScale  = 0.5;
var recipYScale = 2;

// pixels per meter?

var overdrawn = display.overdrawn = function () {
  return aabb(drawnX,drawnY,drawnX + width + bufferX * 2,drawnY + height + bufferY*2);
};

var visible = display.visible = function() {
  var l = drawnX + scrollX;
  var t = drawnY + scrollY;
  return aabb(l,t,l+width,t+height);
};

var snapTo = display.snapTo = function(x,y) {
  var sl = x - drawnX;
  var st = y - drawnY;
  if (sl >= 0 && st >= 0 && sl < bufferX*2 && st < bufferY*2) {
    scrollX = sl;
    scrollY = st;
    console.log("near",drawnX,drawnY,scrollX,scrollY);
  } else {
    dirty = DIRTY_EVERYTHING;
    drawnX = x - bufferX;
    drawnY = y - bufferY;
    scrollX = bufferX;
    scrollY = bufferY;
    console.log("far",drawnX,drawnY,scrollX,scrollY);
  }
};

var snapBy = display.snapBy = function(dx,dy) {
  snapTo(drawnX + scrollX + dx, drawnY + scrollY + dy);
};

var layer = function(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

// order should match the order of the DIRTY_FOO bitmasks above
var layers = display.layers = [ background, shadows, foreground ];

var resized = function() {
  width  = document.body.clientWidth;
  height = document.body.clientHeight;
  for (var i in layers) {
    var l = layers[i];
    l.width(width)
    l.height(height);
  }
  dirty = DIRTY_EVERYTHING;
};

$(window).bind("resize", resized);
$(window).ready(resized);

// window.setInterval( function() { return snapBy(1,1); }, 33);
var render = function() {
  var t = performance.now();
  var pt = physics.updated;

  requestAnimationFrame(render);

  // no physics yet
  if (!pt) return;

  // let physics run ahea of rendering by one frame.
  // use alpha to interpolate between the last 2 physics frames for display for reduced jitter
  // from rendering/physics framerate mismatch.
  // var alpha = t - pt; // Math.max(0, Math.min((t - pt) * 25/ 1000, 1));
  var alpha = (t - pt) / physics.MILLISECONDS_PER_FRAME;

  // if (display.frame % 600 == 0)
  //   console.log("display frame", display.frame, "at", (t/1000).toFixed(3),"off physics frame", physics.frame, "from time", (pt/1000).toFixed(3), "at alpha", alpha.toFixed(2));

  var frame = ++display.frame;

  stats.display.begin();

  // console.log(display.overdrawn(), display.visible());

  dirty = DIRTY_SHADOWS | DIRTY_FOREGROUND;
  // draw the world

  frame = (frame+1) % 60;
  var x = 50; // drawnX + events.mouseX;
  var y = 50; // drawnY + events.mouseY;
  n = Math.sin(frame * 3.14 / 30) * 10 + 10;

  if (dirty & DIRTY_BACKGROUND) {
    var c = background.canvas;
    c.clear(true);
    c.setTransform(1,0,0,1,-drawnX,-drawnY);
  }

  if (dirty & DIRTY_SHADOWS) {
    var c = shadows.canvas;
    c.clear(true);
    c.setTransform(1,0,0,0.5,-drawnX,-drawnY);
    c.shadowColor = "#000000";
    c.shadowOffsetX = 0;
    c.shadowOffsetY = 0;
    c.shadowBlur = 5 + (n/1.5);
    for (var i = 0; i < 200; i ++) {
      n = Math.sin((frame + i*2) * 3.14 / 30) * 10 + 10;
      c.beginPath();
      c.arc(x + Math.floor(i%20)*30,y*2 + (i/20)*60,10,0,2*Math.PI,false);
      c.fillStyle = "rgba(0,0,0,0.25)";
      c.fill();
    }
  }

  if (dirty & DIRTY_FOREGROUND) {
    var c = foreground.canvas;
    c.clear(true);
    c.setTransform(1,0,0,1,-drawnX,-drawnY);
    for (var i =0; i < 200; i++) {
      n = Math.sin((frame + i*2) * 3.14 / 30) * 10 + 10;
      c.beginPath();
      c.arc(x + Math.floor(i%20)*30,y-10-n + (i/20)*30,10,0,2*Math.PI,false);
      var my_gradient=c.createLinearGradient(x,y,x+400,y+400);
      my_gradient.addColorStop(0,"red");
      my_gradient.addColorStop(1,"blue");
      c.fillStyle=my_gradient;
      c.strokeStyle = "black";

      //c.fillStyle = "#3A5BCD";
      c.lineWidth = 0.3;
      c.stroke();
      c.fill();
    }
  }

  dirty = DIRTY_NOTHING;

  stats.display.end();
};

render(performance.now());
});
