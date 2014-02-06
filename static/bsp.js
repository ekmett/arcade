// a basic solid bsp

define([],function() {

var bsp = {
  epsilon : 0.01 // 1cm
};

var Node = bsp.Node = function(nx,ny,nz,d,f,b) {
  this.nx = nx; // normal
  this.ny = ny;
  this.nz = nz;
  this.d  = d;  // distance from origin
  this.front = front;
  this.back  = back;
};

Node.prototype = {
  classify: function(x,y,z) {
    var d = x*this.nx+y*this.ny+z*this.nz+this.d;
    if (d > bsp.epsilon)  return this.front.classify(x,y);
    if (d < -bsp.epsilon) return this.back.classify(x,y);
    return this.front.classify(x,y).concat(this.back.classify(x,y));
  }
};

var Leaf = bsp.Leaf = function(contents) {
  this.contents = contents;
};

Leaf.prototype = {
  classify: function(x,y) { return this.contents; }
};

return bsp;

});
