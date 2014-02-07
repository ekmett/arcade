define([],function() {

var Wall = function(nx,ny,d) {
  this.nx = nx;
  this.ny = ny;
  this.d = d;
  var dir = 0;
  if (this.nx < 0) dir += 1;
  if (this.ny < 0) dir += 2;
  this.dir = dir;
};

Wall.prototype = {
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
};

var wall = function(nx,ny,d) {
  return new Wall(nx,ny,d);
};

wall.Wall = Wall;

return wall;

});
