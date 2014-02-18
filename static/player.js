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

var player = new physics.Particle(0,0,0,0.5,0.5,2,100);
var image = new Image();
image.onload = function() {
  image.complete = true;
  console.log('image loaded');
};
image.src = 'images/sprites/books_what.png';

player.elasticity = 2;
player.graspable = true;
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
    scratch.sx-1.25, scratch.sy-4.75, 2*Math.sqrt(2), 4*Math.sqrt(2)
  );

  s.scale(1,0.5);
  scratch.world(this.rx,this.ry,0);
  s.beginPath();
  s.arc(scratch.sx,scratch.sy*2+this.h*0.60,this.w*Math.sqrt(3),0,2*Math.PI,false);
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

  var m = player.standing ? 1 : (player.bouncing && !player.grasped) ? 0.7 : 0.4;

  // if (player.bouncing) console.log("bouncing");

  if (events.impulse[87]) { pdx -= m; pdy -= m; } // W
  if (events.impulse[65]) { pdx += m; pdy -= m; } // A
  if (events.impulse[83]) { pdx += m; pdy += m; } // S
  if (events.impulse[68]) { pdx -= m; pdy += m; } // D
  if (player.jumpStart) {
    if (events.impulse[32] && (player.standing || (player.bouncing && !player.grasped))) {
      // high jump
      pdz += 1.4;
    } else {
      // low jump
      pdz += 1.2;
    }
    player.jumpStart = false;
  } else if (events.impulse[32] && (player.standing || (player.bouncing && ! player.grasped))) {
    player.jumpStart = true;
  }
  player.push(5*pdx,5*pdy,50*pdz);

  if (events.impulse[81] && this.selected) { // "Q" levitates target
    this.selectedZ += 0.20;
  }

  if (events.impulse[69] && this.selected) { // "E" lowers target
    this.selectedZ -= 0.20;
  }
  if (events.impulse[90] && this.selected) {
    this.selected.lift = 0; // hover forever
    this.selected.priority = 0; // pinned
    this.selectedInverseMass = 0; // pin it forever
  }


  if (events.mouse[1]) {
    if (this.selected) {
      // drag
      this.selected.x = this.selectedX + display.cursor.x;
      this.selected.y = this.selectedY + display.cursor.y;
      this.selected.z = this.selectedZ;
      physics.scene.clip(this.selected);
    } else {
      this.selected = select();
      if (this.selected) {
        this.selectedX = this.selected.rx - display.cursor.x;
        this.selectedY = this.selected.ry - display.cursor.y;
        this.selectedZ = this.selected.rz;
        this.selectedLift = this.selected.lift;
        this.selectedInverseMass = this.selected.inverseMass;
        this.selected.inverseMass = 0;
      }
    }
  } else {
    if (this.selected) {
      this.selected.inverseMass = this.selectedInverseMass;
      this.selected.lift = this.selectedLift;
    }
    this.selected = null;
  }
};


return player;

});

