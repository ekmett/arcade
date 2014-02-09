define([], function () {

var EPSILON = 0.01; // 1cm

return {
  // a spring that forces the distance between the upper right corner of a and b to be l
  stick : function (a,b,l) {
    return function() {
      var dx = a.x - b.x;
      var dy = a.y - b.y;
      var dz = a.z - b.z;
      // TODO: use the Taylor series approximation around l^2 to soften the spring
      var dl = sqrt(dx*dx + dy*dy + dz*dz);
      var ima = a.inverseMass;
      var imb = b.inverseMass;
      var denom = dl*(ima+imb);
      if (denom > EPSILON) {
        var diff = (dl-l)/denom;
        dx *= diff;
        dy *= diff;
        dz *= diff;
        a.x -= dx*ima;
        a.y -= dy*ima;
        a.z -= dz*ima;
        b.x += dx*imb;
        b.y += dy*imb;
        b.z += dz*imb;
      } else {
        console.log("constraints.stick", "near singularity",a,b,l)
      }
    }
  }
};

});
