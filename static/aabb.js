define([], function() {

var AABB = function(x,y,z,w,h,d) {
  this.x = x;
  this.y = y;
  this.z = z;
  this.w = w;
  this.h = h;
  this.d = d;
};

AABB.prototype = {
  center: function() {
    return { x : this.x + this.w / 2
	   , y : this.y + this.h / 2
           , z : this.z + this.d / 2
           };
  },
  area : function() {
    return this.w * this.h;
  },
  volume : function() {
    return this.w * this.h * this.d;
  },
  toString : function() {
    return "aabb(" + x + "," + y + "," + z + "," + w + "," + h + "," + d + ")";
  },
  union : function(that) {
    return new AABB(
      Math.min(this.x,that.x),
      Math.min(this.y,that.y),
      Math.min(this.z,that.z),
      Math.min(this.x+this.w,that.x+that.w),
      Math.min(this.y+this.h,that.y+that.h),
      Math.min(this.z+this.d,that.y+that.d));
  },
  contains : function(x,y,z) {
    return x >= this.x && x <= this.x + this.w &&
           y >= this.y && y <= this.y + this.h &&
           z >= this.z && z <= this.z + this.d;
  },
  overlaps: function(that) {
    return Math.abs(that.x - this.x) <= this.w + that.w &&
           Math.abs(that.y - this.y) <= this.h + that.h
           Math.abs(that.z - this.z) <= this.d + that.d;
  },
  translate: function(dx,dy,dz) {
    return new AABB(this.x + dx, this.y + dy, this.z + dz, this.w, this.h, this.d)
  }
};

var aabb = function(x,y,z,w,h,d) {
  return new AABB(x,y,z,w,h,d);
};

aabb.AABB = AABB;

return aabb;

});
