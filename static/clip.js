define([], function() {

var Velocity = function Velocity(vx,vy,vz) {
  this.vx = vx || 0;
  this.vy = vy || 0;
  this.vz = vz || 0;
};

// annoying definitions are to avoid allocation. -EAK

// used during restitution to stop slow drift
var EPSILON = 0.005; // 5mm/frame ~ 0.3 miles per hour

// the walls in the bsp are strictly 2d
// mutates it.vx, it.vy
var clip2d = function clip2d(nx,ny,bounce) {
  var vx = this.vx;
  var vy = this.vy;

  var backoff = (vx*nx + vy*ny) * bounce;

  vx -= nx*backoff;
  vy -= ny*backoff;

  if (vx >= -EPSILON && vx <= EPSILON) vx = 0;
  if (vy >= -EPSILON && vy <= EPSILON) vy = 0;

  this.vx = vx;
  this.vy = vy;
};

// mutates it.vx, it.vy, it.vz
var clip3d = function clip3d(nx,ny,nz,bounce) {
  var vx = this.vx;
  var vy = this.vy;
  var vz = this.vz;

  var backoff = (vx*nx + vy*ny + vz*nz) * bounce;

  vx -= nx*backoff;
  vy -= ny*backoff;
  vz -= nz*backoff;

  if (vx >= -EPSILON && vx <= EPSILON) vx = 0;
  if (vy >= -EPSILON && vy <= EPSILON) vy = 0;
  if (vz >= -EPSILON && vz <= EPSILON) vz = 0;

  this.vx = vx;
  this.vy = vy;
  this.vz = vz;
};

Velocity.prototype = {
  clip2d: clip2d,
  clip3d: clip3d
};

return {
  clip2d : clip2d, // for patching into others
  clip3d : clip3d, // for patching into others
  Velocity : Velocity,
  epsilon : function(e) {
    if (typeof e !== 'undefined') EPSILON = e;
    return EPSILON;
  }
};

});
