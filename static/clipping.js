define([], function() {

// a swept bounding box.
var Clipping = function(it,x1,y1,z1,x2,y2,z2,bounce) {
  this.box = box; // starting box
  this.dx = dx;
  this.dy = dy;
  this.dz = dz;
  this.aabb = box.union(box.translate(dx,dy,dz));

/*
  // 012
  // 345
  // 678

  var direction = 0;
  if (dx > 0) direction += 2;
  else if (dx === 0) direction += 1;
  if (dy > 0) direction += 6;
  else if (dy === 0) direction += 3;
  if (dz > 0) direction += 18;
  else if (dz === 0) direction += 9;
  this.direction = direction;
*/
};

var clipping = function (box,dx,dy,dz) {
  return new Clipping(box,dx,dy,dz);
};

clipping.Clipping = Clipping;

return clipping;

});

