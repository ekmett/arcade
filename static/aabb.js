define([], function() {

var AABB = function(x,y,z,w,d,h) {
  this.x = x;
  this.y = y;
  this.z = z;
  this.w = w;
  this.d = d;
  this.h = h;
};

AABB.prototype = {
  center: function() {
    return { x : this.x + this.w / 2
           , y : this.y + this.d / 2
           , z : this.z + this.h / 2
           };
  },
  area : function() {
    return this.w * this.d;
  },
  volume : function() {
    return this.w * this.d * this.h;
  },
  toString : function() {
    return "aabb(" + x + "," + y + "," + z + "," + w + "," + d + "," + h + ")";
  },
  union : function(that) {
    return new AABB(
      Math.min(this.x,that.x),
      Math.min(this.y,that.y),
      Math.min(this.z,that.z),
      Math.min(this.x+this.w,that.x+that.w),
      Math.min(this.y+this.d,that.y+that.d),
      Math.min(this.z+this.h,that.y+that.h));
  },
  contains : function(x,y,z) {
    return x >= this.x && x <= this.x + this.w &&
           y >= this.y && y <= this.y + this.d &&
           z >= this.z && z <= this.z + this.h;
  },
  overlaps: function(that) {
    return Math.abs(that.x - this.x) <= this.w + that.w &&
           Math.abs(that.y - this.y) <= this.d + that.d &&
           Math.abs(that.z - this.z) <= this.h + that.h;
  },
  translate: function(dx,dy,dz) {
    return new AABB(this.x + dx, this.y + dy, this.z + dz, this.w, this.d, this.h)
  }
};

var aabb = function(x,y,z,w,d,h) {
  return new AABB(x,y,z,w,d,h);
};

aabb.AABB = AABB;

return aabb;

});
