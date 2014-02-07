define([], function() {

// annoying definitions are to avoid allocation. -EAK

// used during restitution to stop slow drift
var EPSILON = 0.005; // 5mm/frame ~ 0.3 mph

// the walls in the bsp are strictly 2d
// mutates it.vx, it.vy
var clip2d = function(it,nx,ny,bounce) {
  var vx = it.vx;
  var vy = it.vy;

  var backoff = (vx*nx + vy*ny) * bounce;

  vx -= nx*backoff;
  vy -= ny*backoff;

  if (vx >= -EPSILON && vx <= EPSILON) vx = 0;
  if (vy >= -EPSILON && vy <= EPSILON) vy = 0;

  it.vx = vx;
  it.vy = vy;
};

// mutates it.vx, it.vy, it.vz
var clip3d = function(it,nx,ny,nz,bounce) {
  var vx = it.vx;
  var vy = it.vy;
  var vz = it.vz;

  var backoff = (vx*nx + vy*ny + vz*nz) * bounce;

  vx -= nx*backoff;
  vy -= ny*backoff;
  vz -= nz*backoff;

  if (vx >= -EPSILON && vx <= EPSILON) vx = 0;
  if (vy >= -EPSILON && vy <= EPSILON) vy = 0;
  if (vz >= -EPSILON && vz <= EPSILON) vz = 0;

  it.vx = vx;
  it.vy = vy;
  it.vz = vz;
};

return {
  clip2d : clip2d,
  clip3d : clip3d,
  epsilon : function(e) {
    if (typeof e !== 'undefined') EPSILON = e;
    return EPSILON;
  }
};

});
