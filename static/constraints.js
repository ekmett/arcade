define([], function constraints() {

var EPSILON = 0.0001; // 1cm

return {
  // a spring that forces the distance between the upper right corner of a and b to be l
  stick : function stick(a,b,l) {
    return function () {
      var dx = a.x - b.x + (a.w - b.w)/2;
      var dy = a.y - b.y + (a.d - b.d)/2;
      var dz = a.z - b.z + (a.h - b.h)/2;
      // TODO: use the Taylor series approximation around l^2 to soften the spring
      var dl = Math.sqrt(dx*dx + dy*dy + dz*dz);
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
        // console.log("constraints.stick", "near singularity",a,b,l)
      }
    }
  }
};

});
