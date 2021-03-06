define([],function(){

var transformations = {
  scrollX : 0,
  scrollY : 0
};

// to avoid javascript object allocation we stuff transformed coordinates in one of these.
var ScreenPoint = transformations.ScreenPoint = function ScreenPoint (sx,sy) {
  this.sx = +sx;
  this.sy = +sy;
};

// embed from world coordinates: translate, transform, translate. the matrix isn't any simpler operation count-wise.
ScreenPoint.prototype.world = function world(x,y,z) {
  x -= transformations.scrollX; // offset into the world
  y -= transformations.scrollY;
  this.sx = 2*(y-x);
  this.sy = x+y-2*z;
};

ScreenPoint.prototype.worldR = function worldR(p) {
  var x = p.rx, y = p.ry, z = p.rz;
  x -= transformations.scrollX; // offset into the world
  y -= transformations.scrollY;
  this.sx = 2*(y-x);
  this.sy = x+y-2*z;
};


var WorldPoint = transformations.WorldPoint = function WorldPoint (x,y) {
  this.x = +x;
  this.y = +y;
};

WorldPoint.prototype.screen = function screen(sx,sy) {
  this.x = transformations.scrollX - 0.25*sx + 0.5*sy;
  this.y = transformations.scrollY + 0.25*sx + 0.5*sy;
};

WorldPoint.prototype.screenS = function screenS(p) {
  var sx = p.sx, sy = p.sy;
  this.x = transformations.scrollX - 0.25*sx + 0.5*sy;
  this.y = transformations.scrollY + 0.25*sx + 0.5*sy;
};

return transformations;

});
