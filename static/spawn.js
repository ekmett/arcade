define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles", "player","prim","ragdoll"],
  function display(constraints, physics, cc, events, images, transformations, toggles, player, prim, ragdoll) {

var scratch = new transformations.ScreenPoint();

var ball = function ball() {
  var r = -Math.log(Math.random())/2.5+0.2;
  var particle = new physics.Particle(Math.random()*10-5,Math.random()*10-5, Math.random()*10,r,r,r,20*r^3);
  particle.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
  particle.specular = 0.7; // the higher the number the larger the specular highlight.
  particle.draw = function(s,c) {
    c.save();

    if (toggles.bounding) {
      prim.tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
      c.lineWidth = 0.01;
      c.strokeStyle = this.color;
      c.stroke();
    }

    if (toggles.soft_shadows) {
      c.shadowColor = 'rgba(0,0,0,.8)';
      c.shadowBlur = 16*this.w;
      c.shadowOffsetX = 0; // 8*this.w;
      c.shadowOffsetY = 4*this.w;
    }

    c.beginPath();
    scratch.world(this.rx,this.ry,this.rz);
    c.arc(scratch.sx,scratch.sy,this.w*Math.sqrt(3),0,2*Math.PI,false);
    c.fillStyle = this.color;
    var g = c.createRadialGradient(scratch.sx,scratch.sy-this.h*Math.sqrt(2),0.1,scratch.sx,scratch.sy,this.w*Math.sqrt(3));
    g.addColorStop(0,"white");
    g.addColorStop(this.specular,this.color);
    c.fillStyle = g;
    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    c.stroke();
    c.fill();

    // experimental, pretty but slow, shadow
    if (toggles.bounding) {
      prim.cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
      c.lineWidth = 0.01;
      c.strokeStyle = "black";
      c.stroke();
    }

    s.save();
    s.scale(1,0.5);
    s.beginPath();
    scratch.world(this.rx,this.ry,0);
    s.arc(scratch.sx,scratch.sy*2+this.h*2,this.w*Math.sqrt(3),0,2*Math.PI,false);
    s.fillStyle = "rgba(0,0,0,0.25)";
    s.fill();
    s.restore();
    c.restore();
  };
  return particle;
};

var dog_ai = function dog_ai() {
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var dz = player.oz * this.lag + player.z * (1 - this.lag) - this.z;
  var l = Math.sqrt(dx*dx+dy*dy+dz*dz);
  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    dz /= l;
    if (this.standing || this.bouncing) this.push(dx*3,dy*3,this.w*20*dz+Math.random()*20); // this.w^3);
  }
};

var coward_ai = function coward_ai() {
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var l = Math.sqrt(dx*dx+dy*dy)*3;
  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    if (this.standing) this.push(-dx*3,-dy*3,0);
  }
};

var spawnKeys = {
  49: function() { /* 1: inert ball */
    physics.particles.push(ball());
  },
  50: function() { /* 2: yappy dog */
    var b = ball();
    physics.particles.push(b);
    b.color = "#f33";
    b.lag = Math.random()*5-3;
    b.ai = dog_ai;
  },
  51: function() { /* 3: yellow scaredy cat */
    var b = ball();
    physics.particles.push(b);
    b.color = "#ff0";
    b.lag = Math.random()*5-3;
    b.ai = coward_ai;
  },
  52: function() { /* 4: cyan wobbler */
    var b = ball();
    physics.particles.push(b);
    b.color = "#0ff";
    var speed1 = Math.random()*4-1;
    var speed2 = Math.random()*4-1;
    b.start = Math.random()*40;
    b.ai = function() {
      var dx = Math.sin((b.start+physics.frame)*3.14/20*speed1);
      var dy = Math.cos((b.start+physics.frame)*3.14/20*speed2);
      if (b.standing) this.push(dx*2,dy*2,0);
    }
  },
  53: function() { /* 5: tar baby */
    var b = ball();
    physics.particles.push(b);
    b.color = "#000";
    b.specular = 0.1;
    b.inverseMass /= 10;
    b.mass *= 10;
    b.elasticity = 0.01;
    b.constraints = 0;
    b.bump = function(that) {
      if (this.constraints < 4 && !that.constrained) {
        this.constraints++;
        that.constrained = true;
        var l = Math.min(this.w+that.w,this.d+that.d,this.h+that.h)/2;
        physics.constraints.push(constraints.stick(b,that,l*0.9));
      }
    }
  },
  54: function() {
    return ragdoll.spawn() /* 6 ragdoll */
  },
  55: function() {
    var r = ragdoll.spawn() /* 7 zombie */
    r.head.lag = 0;
    r.head.ai = dog_ai;
    r.leftWrist.lag = 2;
    r.leftWrist.ai = dog_ai;
    // r.leftElbow.lag = 1;
    // r.leftElbow.ai = dog_ai;
    // r.rightWrist.lag = 2;
    // r.rightWrist.ai = dog_ai;
    // r.rightElbow.lag = 1;
    // r.rightElbow.ai = dog_ai;
    r.waist.lag = 1;
    r.waist.ai = dog_ai;
  }
};

$(document.body).keypress(function(e) {
   if (e.keyCode in spawnKeys) {
     spawnKeys[e.which](e);
   }
});

return {};

});
