define([], function constraints() {

var EPSILON = 0.0001; // 1cm

return {
  // a spring that forces the distance between the upper right corner of a and b to be l
  stick : function stick(a,b,l,maxl) {
    var result = function result() {
      if (result.inactive) return;
      var dx = a.x - b.x + (a.w - b.w)/2;
      var dy = a.y - b.y + (a.d - b.d)/2;
      var dz = a.z - b.z + (a.h - b.h)/2;
      // TODO: use the Taylor series approximation around l^2 to soften the spring
      var dl = Math.sqrt(dx*dx + dy*dy + dz*dz);
      if (maxl != null && dl > l * maxl) {
	// breakable constraints.
        result.inactive = true;
	return;
      }
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
	// displacement
        dx = (Math.random()-0.5)*(a.w+b.w)*0.2;
        dy = (Math.random()-0.5)*(a.d+b.d)*0.2;
        dz = (Math.random()-0.5)*(a.h+b.h)*0.2;
        a.x -= dx;
        a.y -= dy;
        a.z -= dz;
        b.x += dx;
        b.y += dy;
        b.z += dz;
      }
    };
    return result;
  }
};

});
