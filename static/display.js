define(["jquery", "physics", "shim/raf", "shim/cc", "performance", "stats", "events","images"], function($, physics, raf, cc, performance, stats, events, images) {

var display = {
  updated : null,
  frame : 0
};

var main = $("#main");

var scrollX = 0;
var scrollY = 0;
var width   = 640; // set during resize
var height  = 480; // set during resize

// we're using 2:1 dimetric rather than true isometric.

var wx2sx = -2, wx2sy = 1,  wx2sz = 1; // increased z towards the user
var wy2sx =  2, wy2sy = 1,  wy2sz = 1;
var wz2sx =  0; wz2sy = -2, wz2sz = 1;

/*
function screen(x,y,z) {
  this.sx = 2*y-2*x
  this.sy = x+y-2*z;
  this.sz = x+y+z
}

// monkey patch
physics.Body.prototype.viewport = function() {
  this.minsx = 2(this.rx+this.w) -2*(this.ry-this.d)
  this.minsy = this.rx+        this.ry        -2*(this.rz+this.h);
  this.maxsy = this.rx+this.w+ this.ry+this.d -2*this.rz;
};

*/

var yScale  = 0.5;
var recipYScale = 2;

// pixels per meter?

var layer = function(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

var layers = display.layers = [ background, shadows, foreground ];

var resized = function() {
  width  = document.body.innerWidth;
  height = document.body.innerHeight;
  for (var i in layers) {
    var l = layers[i];
    l.width(width);
    l.height(height);
  }
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

  frame = (frame+1) % 60;
  var x = events.mouseX; // 250; // drawnX + events.mouseX;
  var y = events.mouseY; // 250; // drawnY + events.mouseY;
  var z = Math.sin(frame * 3.14 / 30) * 10 + 10;

  var b = background.canvas;
  b.clear(true);
  b.setTransform(1,0,0,1,0,0);

  var s = shadows.canvas;
  s.clear(true);
  s.setTransform(1,0,0,0.5,0,0);
  s.shadowColor = "#000000";
  s.shadowOffsetX = 0;
  s.shadowOffsetY = 0;
  s.shadowBlur = 5;
  n = Math.sin(frame * 3.14 / 30) * 10 + 10;
  s.beginPath();
  s.arc(x,y*2,10,0,2*Math.PI,false);
  s.fillStyle = "rgba(0,0,0,0.25)";
  s.fill();

  var c = foreground.canvas;
  c.clear(true);
  c.setTransform(1,0,0,1,0,0);
  c.beginPath();
  c.arc(x,y-10-z,10,0,2*Math.PI,false);
  var my_gradient=c.createLinearGradient(x,y,x+40,y+40);
  my_gradient.addColorStop(0,"red");
  my_gradient.addColorStop(1,"blue");
  c.fillStyle=my_gradient;
  c.strokeStyle = "black";
  c.lineWidth = 0.3;
  c.stroke();
  c.fill();

  stats.display.end();
}

render(performance.now());
});
