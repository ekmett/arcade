define([], function() {

// a swept bounding box.
var Clipping = function(box,dx,dy) {
  this.box = box; // starting box
  this.dx = dx;
  this.dy = dy;
  this.aabb = box.union(box.translate(dx,dy));

  // 012
  // 345
  // 678

  var direction = 0;
  if (dx > 0) direction += 2;
  else if (dx === 0) direction += 1;
  if (dy > 0) direction += 6;
  else if (dy === 0) direction += 3;
  this.direction = direction;
};

var clipping = function (box,dx,dy) {
  return new Clipping(box,dx,dy);
};

clipping.Clipping = Clipping;

return clipping;

});

