// a basic solid bsp

define([],function() {

var mu_water  = 0.5; // horizontal mu
var mu_ice    = 0.1;
var mu_ground = 0.2;

var bsp = {
  epsilon : 0.01 // 1cm --
  step    : 0.3  // 1ft, barriers less than a foot high can be stepped over without impacting the sweep.
};

// always store position so that normal has forward (direction 1) toward the camera
// we can number the bsp cells front to back
// then we can sort all bodies by bsp cell # and then internally by z to get a good ordering

var Node = bsp.Node = function(nx,ny,d,f,b) {
  this.nx = nx; // normal
  this.ny = ny;
  this.d  = d;  // distance from origin
  this.front = front;
  this.back  = back;

  // updated and used solely by the _renderer_
  this.bodies = null;

  var dir = 0;
  if (this.nx < 0) dir |= 1
  if (this.ny < 0) dir |= 2;
  this.dir = dir;

  // check the aabb of the body against this
};

Node.prototype = {
  distance: function(x,y) {
    return x*this.nx+y*this.ny+this.d;
  },

  // classify a bounding box, borrowing the old quake trick
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
    };
    var result = 0;
    if (d1 >= d) result = 1
    if (d2 > d) result |= 2;
    return result;
  }

  // uses current location, not the whole move
  local: function(body) {
    var r = this.side(body.x,body.y,body.x+body.w,body.x+body.d);
    if (r & 1) this.f.local(body);
    if (r & 2) this.b.local(body);
  }
  clip: function(body) {
    switch (this.side(body.minx,body.miny,body.maxx,body.maxy)) {
      case 1: return this.f.clip(body);
      case 2: return this.b.clip(body);
      case 3:
        // determine starting side
        // clip to it
        // clip to the other if it passes.
        // check step height, if too high, then come back and clip here
        return;
      default: // the impossibe happened;
        return;
    }
    // TODO: clip with the swept bounding box.
  }
};

var Leaf = bsp.Leaf = function(z, mu_h, mu_v) {
  this.z = z;
  this.mu_h = mu_h;
  this.mu_v = mu_v;
};

Leaf.prototype = {
  clip: function(body) {
    // elevate us to floor height
    body.z = Math.max(body.z, this.z);
  }
  // update friction in the scratch pad
  local: function(body) {
     body.mu_h = Math.max(body.mu_h, body.z < this.z + step ? this.mu_h, this.mu_v); // use air drag if not on ground
     body.mu_v = Math.max(body.mu_v, this.mu_v);
  }
};

// an empty BSP, with just the ground and some floor friction for testing
var Empty = bsp.Empty = new Leaf(0, 0.2, 0.01);

return bsp;

});
