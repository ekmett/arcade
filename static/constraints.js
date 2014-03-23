define([], function constraints() {

var EPSILON = 0.0001; // 1cm

return {
  // a spring that forces the distance between the upper right corner of a and b to be l
  stick : function stick(a,b,l,maxl,str) {
    var result = function result(k) {
      if (result.inactive) {
        return;
      }
      var dx = a.x - b.x + (result.ox || (a.w - b.w)/2);
      var dy = a.y - b.y + (result.oy || (a.d - b.d)/2);
      var dz = a.z - b.z + (result.oz || (a.h - b.h)/2);
      // TODO: use the Taylor series approximation around l^2 to soften the spring
      var dl = Math.sqrt(dx*dx + dy*dy + dz*dz);
      if (maxl != null && dl > l * maxl) {
        // break the constraint if it is too far out of whack
        result.inactive = true;
        result.onto && --result.onto.grasped;
        return;
      }
      if (str && k > str) return;
      var ima = a.inverseMass;
      var imb = b.inverseMass;
      if (!ima && !imb) return;
      var denom = dl*(ima+imb);
      if (denom > EPSILON) {
        var diff = (dl-l)/denom;
        dx *= diff;
        dy *= diff;
        dz *= diff;
        if (a.priority >= b.priority) {
          a.x -= dx*ima;
          a.y -= dy*ima;
          a.z -= dz*ima;
        }
        if (a.priority <= b.priority) {
          b.x += dx*imb;
          b.y += dy*imb;
          b.z += dz*imb;
        }
      } else {
	// randomized displacement
        dx = (Math.random()-0.5)*(a.w+b.w)*0.2;
        dy = (Math.random()-0.5)*(a.d+b.d)*0.2;
        dz = (Math.random()-0.5)*(a.h+b.h)*0.2;
        if (a.priority >= b.priority) {
          a.x -= dx;
          a.y -= dy;
          a.z -= dz;
        }
        if (a.priority <= b.priority) {
          b.x += dx;
          b.y += dy;
          b.z += dz;
        }
      }
    };
    return result;
  }
};

});
