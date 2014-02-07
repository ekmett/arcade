// a basic solid bsp

define([],function() {

var mu_air    = 0.01;
var mu_water  = 0.5; // horizontal mu
var mu_ice    = 0.1;
var mu_ground = 0.2;

var Air = {
  mu_h : mu_air,
  mu_v : mu_air,
  ax: 0, ay: 0, az: 0
};

var Ice = {
  mu_h : mu_ice,
  mu_v : mu_air,
  ax: 0, ay: 0, az: 0
};

var Ground = {
  mu_h : mu_ground,
  mu_v : mu_air,
  ax: 0, ay: 0, az: 0
};

var bsp = {
  epsilon : 0.01 // 1cm
  step    : 0.3  // 1ft, barriers less than a foot high can be stepped over without impacting the sweep.
};

// we can always position the normal so that forward is toward the camera, since it is fixed axonometric
// we can number the bsp cells front to back
// then we can sort all bodies by bsp cell # and then internally by z to get a good ordering

var Node = bsp.Node = function(nx,ny,d,f,b) {
  this.nx = nx; // normal
  this.ny = ny;
  this.d  = d;  // distance from origin
  this.front = front;
  this.back  = back;

  var dir = 0;
  if (this.nx < 0) dir += 1
  if (this.ny < 0) dir += 2;
  this.dir = dir;

  // check the aabb of the body against this
};

Node.prototype = {
  distance: function(x,y,z) {
    return x*this.nx+y*this.ny+z*this.nz+this.d;
  },

  // classify a bounding box
  side : function(x1,y1,x2,y2) {
    var d1 = 0, d2 = 0;
    switch (this.dir) {
      case 0:
        d1 = this.nx*x2 + this.ny*y2;
        d2 = this.nx*x1 + this.ny*y1;
        break;
      case 1:
        d1 = this.nx*x1 + this.ny*y2;
        d2 = this.nx*x2 + this.ny*y1;
        break;
      case 2:
        d1 = this.nx*x2 + this.ny*y1;
        d2 = this.nx*x1 + this.ny*y2;
        break;
      case 3:
        d1 = this.nx*x1 + this.ny*y1;
        d2 = this.nx*x2 + this.ny*y2;
        break;
      default:
        console.error("Wall.side: bad dir");
        break;
    }

    var result = 0;
    if (d1 >= d) result = 1
    if (d2 > d) result |= 2;
    return result;
  }

  local: function(acc,x1,y1,x2,y2) {
     var r = this.side(x1,y1,x2,y2);
     switch (this.side(x1,y1,x2,y2)) {
       case 1:
         this.f.local(acc,x1,y1,x2,y2);
         return;
       case 2:
         this.b.local(acc,x1,y1,x2,y2);
         return;
       case 3:
         this.f.local(acc,x1,y1,x2,y2);
         this.b.local(acc,x1,y1,x2,y2);
         return;
       default:
         return;
     }
     // lots of work

  }
  local: function(body) {
    // identify some properties for a given body based on the _current_ location x, y, z
    return { mu_h : mu_ground, mu_v : mu_air, G : G }
  },
  clip: function(body) {
  }
};

var Leaf = bsp.Leaf = function(air,ground,z) {
  this.air = air;
  this.ground = groun;
  this.z = z;
};

Leaf.prototype = {
  clip: function(body) {
    // elevate to floor height
    body.z = Math.max(body.z, this.z);
  }
  local: function(body) {
     if (body.z < this.z + step) return this.ground;
     else return this.air;
  }
};

var Open = bsp.Open = function(floor, ceiling) {
};

return bsp;

});
