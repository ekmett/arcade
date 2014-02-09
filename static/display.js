define(["jquery", "physics", "shim/raf", "shim/cc", "performance", "stats", "events","images"], function display($, physics, raf, cc, performance, stats, events, images) {

var display = {
  updated : null,
  frame : 0
};

var main = $("#main");

var scrollX = 0;
var scrollY = 0;

var width   = 800; // set during resize
var height  = 400; // set during resize

// this lets us see '25 meters x 25 meters' in world space in a 2:1 dimetric projection

var halfWidth = 800;
var halfHeight = 400;

var PIXELS_PER_METER = 32;
var METERS_PER_PIXEL = 1 / PIXELS_PER_METER;

var Y_SCALE  = 0.5;
var RECIP_Y_SCALE = 2;


var layer = function layer(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

var layers = display.layers = [ background, shadows, foreground ];

var playarea = $(".playarea");

var c = foreground.canvas; // most things draw here, give it a short name

c.moveTo3d = function moveTo3d(x,y,z) {
  x -= scrollX; // offset into the world
  y -= scrollY;
  this.moveTo(2*(y-x), x+y-2*z);
};

c.lineTo3d = function lineTo3d(x,y,z) {
  x -= scrollX; // offset into the world
  y -= scrollY;
  this.lineTo(2*(y-x), x+y-2*z);
};

// to avoid javascript object allocation we stuff transformed coordinates in one of these.
var ScreenPoint = function ScreenPoint (sx,sy) {
  this.sx = +sx;
  this.sy = +sy;
};

// embed from world coordinates: translate, transform, translate. the matrix isn't any simpler operation count-wise.
ScreenPoint.prototype.world = function world(x,y,z) {
  x -= scrollX; // offset into the world
  y -= scrollY;
  this.sx = 2*(y-x);
  this.sy = x+y-2*z;
};

var WorldPoint = function WorldPoint (x,y) {
  this.x = +x;
  this.y = +y;
};

WorldPoint.prototype.screen = function screen(sx,sy) {
  this.x = scrollX - 0.25*sx + 0.5*sy;
  this.y = scrollY + 0.25*sx + 0.5*sy;
};

var resized = function resized() {
  width  = Math.min(document.body.clientWidth, 800);
  height = Math.min((document.body.clientHeight) - foreground.offset().left, 400);
  halfWidth = width / 2;
  halfHeight = height / 2;
  playarea.each(function(i,e) {
    $(e).width(width);
    $(e).height(height);
  });
  console.log("play area resized to",width,height);
};

$(window).bind("resize", resized);
$(window).ready(resized);

var tack = function tack(c,x,y,z,w,d,h) {
  x -= scrollX;
  y -= scrollY;

  var oz = -2*z;
  var sx = 2*(y-x), sy = y+x+oz;

  c.beginPath();
  c.moveTo(sx,sy);
  c.lineTo(sx-2*w,sy+w);
  c.moveTo(sx,sy);
  c.lineTo(sx+2*d,sy+d);
  c.moveTo(sx,sy);
  c.lineTo(sx,sy-2*h);
};

var cube = function cube(c,x1,y1,z,w,d,h) {
  x1 -= scrollX; // offset into the world
  y1 -= scrollY;

  var x2 = x1 + w;
  var y2 = y1 + d;

  var oz = -2*z;

  // these can be optimized
  var sx1 = 2*(y1-x1), sy1 = y1+x1+oz;
  var sx2 = 2*(y1-x2), sy2 = y1+x2+oz;
  var sx3 = 2*(y2-x2), sy3 = y2+x2+oz;
  var sx4 = 2*(y2-x1), sy4 = y2+x1+oz;
  var up = -2*h;

  c.beginPath();
  c.moveTo(sx1,sy1);
  c.lineTo(sx2,sy2);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
  c.lineTo(sx1,sy1);
  c.lineTo(sx1,sy1+up);
  c.lineTo(sx2,sy2+up);
  c.lineTo(sx3,sy3+up);
  c.lineTo(sx4,sy4+up);
  c.lineTo(sx1,sy1+up);
  c.moveTo(sx2,sy2);
  c.lineTo(sx2,sy2+up);
  c.moveTo(sx3,sy3);
  c.lineTo(sx3,sy3+up);
  c.moveTo(sx4,sy4);
  c.lineTo(sx4,sy4+up);
};

var cursor = new WorldPoint();

// window.setInterval( function() { return snapBy(1,1); }, 33);
var render = function render() {
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

  var frame = physics.frame; // tie animation to physics frame rate, not drawing rate

  stats.display.begin();

  frame = (frame+1) % 40;
  var x = (events.mouseX-halfWidth) * METERS_PER_PIXEL;
  var y = (events.mouseY-halfHeight) * METERS_PER_PIXEL;

  // update the cursor in world coordinates
  cursor.screen(
    (events.mouseX-halfWidth) * METERS_PER_PIXEL,
    (events.mouseY-halfHeight) * METERS_PER_PIXEL
  );

  /* console.log(x,y); */
  var z = Math.sin(frame * 3.14 / 20) * 1 + 1;

  var b = background.canvas;
  b.clear(true);
  b.setTransform(1,0,0,1,0,0);

  var s = shadows.canvas;
  s.clear(true);
  s.setTransform(PIXELS_PER_METER,0,0,0.5*PIXELS_PER_METER,halfWidth,halfHeight);
  s.beginPath();
  s.arc(x,y*2,1,0,2*Math.PI,false);
  s.fillStyle = "rgba(0,0,0,0.25)";
  s.fill();

  var c = foreground.canvas;
  c.clear(true);
  c.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);
  c.beginPath();
  c.arc(x,y-1-z,1,0,2*Math.PI,false);
  c.fillStyle = "#ff0000";
  c.strokeStyle = "black";
  c.lineWidth = 0.03;
  c.stroke();
  c.fill();

  tack(c,0,0,0,1,1,1);
  c.strokeStyle = "#0f0";
  c.stroke();

  cube(c,cursor.x-0.5,cursor.y-0.5,0,1,1,2);
  c.strokeStyle = "#3cc";
  c.stroke();


  stats.display.end();
}

render(performance.now());

return {
};

});
