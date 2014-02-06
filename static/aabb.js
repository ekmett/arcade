define([], function() {

var AABB = function(x,y,w,h) {
  this.x = x;
  this.y = y;
  this.w = w;
  this.h = h;
};

AABB.prototype = {
  center: function() {
    return { x : this.x + this.w / 2, y : this.y + this.h / 2 }
  },
  area : function() {
    return this.w * this.h;
  },
  toString : function() {
    return "aabb(" + x + "," + y + "," + w + "," + h + ")";
  },
  union : function(that) {
    return new AABB(Math.min(this.x,that.x), Math.min(this.y,that.y), Math.min(this.x+this.w, that.x+that.w), Math.min(this.y+this.h,that.y+that.h));
  },
  contains : function(x,y) {
    return x >= this.x && x <= this.x + this.w && y >= this.y && y <= this.y + this.h;
  },
  overlaps: function(that) {
    return Math.abs(that.x - this.x) <= this.w + that.w &&
           Math.abs(that.y - this.y) <= this.h + that.h
  },
  translate: function(dx,dy) {
    return new AABB(this.x + dx, this.y + dy, this.w, this.h)
  }
};

var aabb = function(x1,y1,x2,y2) {
  return new AABB(x1,y1,x2,y2);
};

aabb.AABB = AABB;

return aabb;

});
