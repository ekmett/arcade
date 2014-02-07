// a basic solid bsp

define([],function() {

var Edge = bsp.Edge = function(front,back) {
  this.front = front;
  this.back = back;
};

var bsp = {
  epsilon : 0.01 // 1cm
};

// we can always position the normal so that forward is toward the camera, since it is fixed axonometric
// we can number the bsp cells front to back
// then we can sort all bodies by bsp cell # and then internally by z to get a good ordering
// on the plus side the sort by bsp can be a mutable mess.

var Node = bsp.Node = function(nx,ny,d,f,b) {
  this.nx = nx; // normal
  this.ny = ny;
  this.d  = d;  // distance from origin
  this.front = front;
  this.back  = back;
  this.bodes = [];
};

Node.prototype = {
  classify: function(x,y,z) {
    var d = x*this.nx+y*this.ny+z*this.nz+this.d;
    if (d > bsp.epsilon)  return this.front.classify(x,y);
    if (d < -bsp.epsilon) return this.back.classify(x,y);
    var f = this.front.classify(x,y);
    var b = this.back.classify(x,y);
    return edge(f,b);
  },
  draw: function(x,y,z) {
  }
};

var Leaf = bsp.Leaf = function(contents) {
  this.contents = {};
};

Leaf.prototype = {
  classify: function(x,y) { return this.contents; }
};

var Open = bsp.Open = function(floor, ceiling) {
};

return bsp;

});
