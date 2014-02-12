define([], function() {

// cheesy display primitives

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
  c.moveTo(sx3,sy3);
  c.lineTo(sx2,sy2);
  c.lineTo(sx2,sy2+up);
  c.lineTo(sx3,sy3+up);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
  c.lineTo(sx4,sy4+up);
  c.lineTo(sx1,sy1+up);
  c.lineTo(sx2,sy2+up);
  c.moveTo(sx3,sy3+up);
  c.lineTo(sx4,sy4+up);
};

var room = function cube(c,x1,y1,z,w,d,h) {
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
  c.moveTo(sx1,sy1+up);
  c.lineTo(sx2,sy2+up);
  c.lineTo(sx2,sy2);
  c.lineTo(sx1,sy1);
  c.lineTo(sx1,sy1+up);
  c.lineTo(sx4,sy4+up);
  c.lineTo(sx4,sy4);
  c.lineTo(sx1,sy1);
  c.moveTo(sx2,sy2);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
};

var floor = function floor(c,x1,y1,z,w,d,h) {
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
};

return {
  /* testing primitives */
  tack  : tack,
  cube  : cube,
  room  : room,
  floor : floor
};

});
