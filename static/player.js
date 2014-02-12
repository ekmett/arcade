define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles","prim"],
  function display(constraints, physics, cc, events, images, transformations, toggles,prim) {

var scratch = new transformations.ScreenPoint();

var player = new physics.Particle( 0,0,0,0.5,0.5,2,100);
var image = new Image();
image.onload = function() {
  image.complete = true;
  console.log('image loaded');
};
image.src = 'images/sprites/books_what.png';

player.elasticity = 0.8;
player.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
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

player.ai = function() {
  var pdx = 0;
  var pdy = 0;
  var pdz = 0;

  var m = player.standing ? 1 : player.bouncing ? 0.7 : 0.2;

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
};


return player;

});

