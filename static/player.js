define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles","prim","display"],
  function display(constraints, physics, cc, events, images, transformations, toggles,prim,display) {

var scratch = new transformations.ScreenPoint();

// pick the item under the mouse cursor, if any
function select() {
  scratch.world(display.cursor.x, display.cursor.y,0);
  var hit = null
  var hit_d = -1000;
  for (i in physics.particles) {
    var p = physics.particles[i];
    var d = p.pick(scratch.sx, scratch.sy);
    if (d != null && d > hit_d) {
      hit = p;
      hit_d = d;
    }
  }
  return hit;
}

var player = new physics.Particle( 0,0,0,0.5,0.5,2,100);
var image = new Image();
image.onload = function() {
  image.complete = true;
  console.log('image loaded');
};
image.src = 'images/sprites/books_what.png';

player.elasticity = 0.8;
physics.particles.push(player);
player.draw = function(s,c) {
  c.save();
  s.save();

  // local blur aroun the model
  if (toggles.soft_shadows) {
    c.shadowBlur = 16*this.w;
    c.shadowColor = 'rgba(0,0,0,.8)';
    c.shadowOffsetX = 0; // 8*this.w;
    c.shadowOffsetY = 0; // 4*this.w;
  }

  if (toggles.bounding) {
    prim.tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
    c.lineWidth = 0.01;
    c.strokeStyle = "grey";
    c.stroke();
  }

  scratch.world(this.rx,this.ry,this.rz);
  c.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight,
    scratch.sx-1.25, scratch.sy-5.5, 2*Math.sqrt(3), 4*Math.sqrt(3)
  );

  s.scale(0,0.5);
  scratch.world(this.rx,this.ry,0);
  s.beginPath();
  s.arc(scratch.sx,scratch.sy*2+this.h,this.w*0.5*Math.sqrt(3),0,2*Math.PI,false);
  s.fillStyle = "rgba(0,0,0,0.25)";
  s.fill();

  if (toggles.bounding) {
    prim.cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
    c.lineWidth = 0.01;
    c.strokeStyle = "black";
    c.stroke();
  }

  c.restore();
  s.restore();
};

// you can't pick yourself for now.
player.pick = function(x,y) { return null };

player.ai = function() {
  var pdx = 0;
  var pdy = 0;
  var pdz = 0;

  var m = player.standing ? 1 : player.bouncing ? 0.7 : 0.4;

  if (player.bouncing) console.log("bouncing");

  if (events.impulse[87]) { pdx -= m; pdy -= m; } // W
  if (events.impulse[65]) { pdx += m; pdy -= m; } // A
  if (events.impulse[83]) { pdx += m; pdy += m; } // S
  if (events.impulse[68]) { pdx -= m; pdy += m; } // D
  if (player.jumpStart) {
    if (events.impulse[32] && (player.standing || player.bouncing)) {
      // high jump
      pdz += 1.4;
    } else {
      // low jump
      pdz += 1.2;
    }
    player.jumpStart = false;
  } else if (events.impulse[32] && (player.standing || player.bouncing)) {
    player.jumpStart = true;
  }
  player.push(5*pdx,5*pdy,50*pdz);

  if (events.impulse[81] && this.selected) { // "Q" levitates target
    this.selectedZ += 0.1;
  }
  if (events.impulse[69] && this.selected) { // "E" lowers target
    this.selectedZ -= 0.1;
  }

/*
  if (events.impulse[9] && this.selected) {
    this.selected.inverseMass = this.selectedInverseMass;
    var s = this.selected;
    this.selected = null;
    s.push(5*(s.x - display.cursor.x), 5*(s.y - display.cursor.y), 50);
  }
*/

  if (events.mouse[1]) {
    if (this.selected) {
      // drag
      this.selected.x = this.selectedX + display.cursor.x;
      this.selected.y = this.selectedY + display.cursor.y;
      this.selected.z = this.selectedZ;
      this.selected.az += physics.G;
      physics.scene.clip(this.selected);
    } else {
      this.selected = select();
      if (this.selected) {
        this.selectedX = this.selected.rx - display.cursor.x;
        this.selectedY = this.selected.ry - display.cursor.y;
        this.selectedZ = this.selected.rz;
        this.selectedInverseMass = this.selected.inverseMass;
        this.selected.inverseMass = 0;
      }
    }
  } else {
    if (this.selected) {
      this.selected.inverseMass = this.selectedInverseMass;
    }
    this.selected = null;
  }

 /*
  if (events.mouse[1]) {
    // this.shotAt = events.mouse[1];
    var f = 3;//e18;
    var fx = f, fy = f, fz = f;
    var x = display.cursor.x;
    var y = display.cursor.y;
    var z = 1;
    var ps = physics.particles; // TODO: just check buckets around (x,y)?
    var dx = x - player.x, dy = y - player.y, dz = 0;
    var d = Math.sqrt(dx*dx +dy*dy + dz*dz);
    if (d < 0.0001) return;
    for (var i in ps) {
      var p = ps[i];
      var dpx = p.x - player.x, dpy = p.y - player.y, dpz = p.z - player.z;
      var dp = Math.sqrt(dpx*dpx+dpy*dpy+dpz*dpz);
      if (dp < 0.0001) continue;
      dpx /= dp;
      dpy /= dp;
      dpz /= dp;

      var dc = Math.sqrt(dpx*dx +dpy*dy + dpz*dz); // dot product of direction
      if (dc > 0.9659 && !Object.is(p,player)) // 30 degree arc
        p.push(dpx,dpy,dpz); // add fall off?
    }
  }
  */
};


return player;

});

