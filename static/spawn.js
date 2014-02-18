define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles", "player","prim","ragdoll"],
  function display(constraints, physics, cc, events, images, transformations, toggles, player, prim, ragdoll) {

var scratch = new transformations.ScreenPoint();
var scratch2 = new transformations.ScreenPoint();

var SCENE_WIDTH  = physics.SCENE_WIDTH;
var SCENE_HEIGHT = physics.SCENE_HEIGHT;
var SCENE_DEPTH  = physics.SCENE_DEPTH;


var ball = function ball() {
  var r = -Math.log(Math.random())/3+0.2;
  var particle = new physics.Particle(
    Math.random()*(SCENE_WIDTH-r)-SCENE_WIDTH/2,
    Math.random()*(SCENE_DEPTH-r)-SCENE_DEPTH/2,
    Math.random()*(SCENE_HEIGHT-r),
    r,r,r,40*r^2.8
  );
  particle.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
  particle.specular = 0.7; // the higher the number the larger the specular highlight.
  particle.pick = function(x,y) {
    scratch.world(this.rx,this.ry,this.rz);
    var dx = scratch.sx-x;
    var dy = scratch.sy-y;
    var near = dx*dx + dy*dy < this.w * this.w * 3;
    return near ? this.key : null;
  };
  particle.draw = function(s,c) {
    c.save();

    if (toggles.bounding) {
      prim.tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
      c.lineWidth = 0.01;
      c.strokeStyle = this.color;
      c.stroke();
    }

    if (toggles.soft_shadows) {
      c.shadowColor = 'rgba(0,0,0,.6)';
      c.shadowBlur = 16*this.w;
      c.shadowOffsetX = 0; // 8*this.w;
      c.shadowOffsetY = 4*this.w;
    }

    c.beginPath();
    scratch.world(this.rx,this.ry,this.rz);
    c.arc(scratch.sx,scratch.sy,this.w*Math.sqrt(3),0,2*Math.PI,false);
    c.fillStyle = this.color;
    c.strokeStyle = "#555";

    var g = c.createRadialGradient(scratch.sx,scratch.sy-this.h*Math.sqrt(2),0.1,scratch.sx,scratch.sy,this.w*Math.sqrt(3));
    c.globalAlpha = 0.88;
    g.addColorStop(0,"white");
    g.addColorStop(this.specular,this.color); // "black");
    c.fillStyle = g;
    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    c.stroke();
    c.fill();
    c.globalAlpha = 1;

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
    s.arc(scratch.sx,scratch.sy*2+this.h*2,this.w*Math.sqrt(2),0,2*Math.PI,false);
    s.fillStyle = "rgba(0,0,0,0.25)";
    s.fill();
    s.restore();
    c.restore();
  };
  // particle.outlets = Math.floor(particle.w/0.3);
  return particle;
};

var Cable = function Cable(l) {
  var r = Math.random()*0.1+0.28;
  var x0 = Math.random()*10-5;
  var a0 = Math.random()*Math.PI*2;
  var s0 = Math.sin(a0);
  var c0 = Math.cos(a0);
  l = l || Math.floor(Math.random()*6+2);
  var color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
  // var color = '#383';
  var ps = new Array(l);
  for (var i=0;i<l;i++) {
    var y0 = r*1.5*i-.75*r*l;
    ps[i] = new physics.Particle(x0*s0+y0*c0,x0*c0-y0*s0,0,r,r,r,13.5*r^2);
    ps[i].draw = function(s,c) {
      if (toggles.bounding) {
        c.save();
        prim.tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
        c.lineWidth = 0.01;
        c.strokeStyle = this.color;
        c.stroke();
        c.restore();
      }
    };
    ps[i].elasticity = 1;
    ps[i].pick = function(x,y) {
      scratch.world(this.rx,this.ry,this.rz);
      var dx = scratch.sx-x;
      var dy = scratch.sy-y;
      var near = dx*dx + dy*dy < this.w * this.w * 3;
      return near ? this.key : null;
    };
    physics.particles.push(ps[i]);
  }

  var oldDraw = ps[Math.floor(l/2)].draw;
  ps[Math.floor(l/2)].draw = function(s,c) {
    c.save();
    s.save();

    c.beginPath();
    s.beginPath();
    scratch.world(ps[0].rx,ps[0].ry,ps[0].rz);
    c.moveTo(scratch.sx,scratch.sy);
    s.moveTo(scratch.sx,scratch.sy+ps[0].rz*2);

    var n = ps.length - 2;
    var u = scratch, v = scratch2;
    u.worldR(ps[1]);
    for (var i=1;i<n;i++) {
      v.worldR(ps[i+1]);
      var x = (u.sx + v.sx) * 0.5;
      var y = (u.sy + v.sy) * 0.5;
      c.quadraticCurveTo(u.sx, u.sy, x, y);
      s.quadraticCurveTo(u.sx, u.sy + ps[i].rz*2 + ps[i].h/2,
                         x   , y + ps[i].rz + (ps[i].h + ps[i+1].h)/4 + ps[i+1].rz);
      var t = u; u = v; v = t;
    }
    v.worldR(ps[n+1]);
    c.quadraticCurveTo(u.sx, u.sy, v.sx, v.sy);
    s.quadraticCurveTo(u.sx, u.sy + (ps[n].rz*2 + ps[n].h/2), v.sx, v.sy + ps[n+1].rz*2 + ps[n+1].h/2);
    c.lineCap = 'round';
    c.lineJoin = 'round';
    c.lineWidth = r+0.05;
    c.strokeStyle = "#000";
    c.stroke();
    c.lineWidth = r-0.05;
    c.strokeStyle = color;
    c.stroke();
    c.lineWidth = 0.05; // offset upwards please
    c.globalCompositeOperation = "lighter";
    c.strokeStyle = "#555";
    c.stroke();
    c.globalCompositeOperation = "source-over";

    s.lineCap = 'round';
    c.lineJoin = 'round';
    s.lineWidth = r;
    s.strokeStyle = "rgba(0,0,0,0.25)";
    s.stroke();

    oldDraw.call(this,s,c);
    c.restore();
    s.restore();
  };

  this.parts = ps;
  this.radius = r;

  for (var i=0;i<l-1;++i) {
    physics.constraints.push(ragdoll.auto(ps[i],ps[i+1]));
  }

};


var snake_ai = function snake_ai() {
  // if (Math.random() >= this.vigor) return;
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var dz = player.oz * this.lag + player.z * (1 - this.lag) - this.z + player.h / 2;
  var l = Math.sqrt(dx*dx+dy*dy+dz*dz);
  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    dz /= l;
    this.intendedX += dx*(this.vscale||1)*(0.5+Math.random()*Math.random())*this.bounce;
    this.intendedY += dy*(this.vscale||1)*(0.5+Math.random()*Math.random())*this.bounce;
    this.intendedZ += Math.random() < 0.5 ? dz*(this.vscale||1)*this.bounce*2 : 0;
  }
  this.push(this.intendedX || 0, this.intendedY || 0, this.intendedZ || 0);
  this.intendedX *= 0.4;
  this.intendedY *= 0.4;
  this.intendedZ *= 0.4;
};


var snake = function() {
  var c = new Cable();
  var r = c.radius;
  var ps = new Cable().parts;

  var lag = Math.random()*2;
  ps[0].ai = snake_ai;
  ps[0].bounce = r*r*6;
  ps[0].lag = lag + 4;
  ps[0].vigor = 0.6;
  ps[0].vscale = 1;
  ps[0].intendedX = 0;
  ps[0].intendedY = 0;
  ps[0].intendedZ = 0;

  ps[3].ai = snake_ai;
  ps[3].bounce = r*r*2;
  ps[3].lag = lag + 6;
  ps[3].vscale = 1;
  ps[3].vigor = 0.2;
  ps[3].intendedX = 0;
  ps[3].intendedY = 0;
  ps[3].intendedZ = 0;
};

var dog_ai = function dog_ai() {
  // if (Math.random() >= this.vigor) return;
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var dz = player.oz * this.lag + player.z * (1 - this.lag) - this.z;
  var l = Math.sqrt(dx*dx+dy*dy+dz*dz);

  if (this.grasping && (Math.random() < (this.releaseFrequency || 0.95))) {
    this.grasping.inactive = true;
    this.grasping = null;
  }

  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    dz /= l;
    if (this.standing || this.bouncing) this.push(dx*(Math.random()-.05)*(this.vscale||2),dy*(Math.random()-.05)*(this.vscale||2),this.w*this.bounce*dz+Math.random()*this.bounce);
  }
};

var claw_ai = function claw_ai() {
  if (Math.random() >= this.vigor) return;
  var dx = (player.ox * this.lag + player.x * (1 - this.lag) - this.x) * this.sign;
  var dy = (player.oy * this.lag + player.y * (1 - this.lag) - this.y) * this.sign;
  var l = Math.sqrt(dx*dx+dy*dy);

  if (this.grasping && (Math.random() < (this.releaseFrequency || 0.95))) {
    this.grasping.inactive = true;
    this.grasping = null;
  }

  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    if (this.standing) this.push(dx*2,dy*2,Math.random());
  }
};

var plug = function plug(that) {
  if (!this.plugged && that.outlets > 0) {
    this.plugged = true;
    that.outlets--;
    var l = Math.min(that.w,that.d,that.h)/2;
    physics.constraints.push(constraints.stick(this,that,l));
  }
}

var grasp = function grasp(that) {
  if (!this.grasping && that.graspable == true && Math.random() < (this.graspFrequency || 0.9)) {
    var l = Math.min(that.w,that.d,that.h);
    var l2 = Math.max(that.w,that.d,that.h);
    this.grasping = constraints.stick(this,that,l,l2/l*2);
    physics.constraints.push(this.grasping);
  }
}

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
    b.bounce = 10;
    b.vigor = 1;
  },
  51: function() { /* 3: yellow scaredy cat */
    var b = ball();
    physics.particles.push(b);
    b.color = "#ff0";
    b.lag = Math.random()*5-3;
    b.ai = claw_ai;
    b.sign = -1;
    b.vigor = 1;
  },
  52: function() { /* 4: cyan balloon */
    var b = ball();
    physics.particles.push(b);
    b.color = "#0ff";
    var speed1 = Math.random()*0.1-0.05;
    var speed2 = Math.random()*0.1-0.05;
    b.start = Math.random()*40;
    b.ai = function() {
      var dx = Math.sin((b.start+physics.frame)*3.14/20)*speed1;
      var dy = Math.cos((b.start+physics.frame)*3.14/20)*speed2;
      this.push(dx*2,dy*2,10*this.w*this.d*10); // lift proportional to surface area.
      this.outlets = 1;
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
      if (this.constraints < 15 && !that.constrained) {
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
    var r = ragdoll.spawn(); /* 7 zombie */
    var ai = dog_ai;
    var loc = ["shoulder","head","leftElbow","rightElbow"];
    for (var i in loc) {
      if (Math.random() < 0.5) {
        r[loc[i]].lag = Math.random()*5-3;
        r[loc[i]].ai = ai;
        r[loc[i]].vigor = 0.05;
        r[loc[i]].bounce = 4;
      }
    }
    r.leftWrist.lag = Math.random()*5-3;
    r.leftWrist.ai = claw_ai;
    r.leftWrist.vigor = 0.1;
    r.leftWrist.sign = 1;
    r.leftWrist.bump = grasp;
    if (Math.random() < 0.5) {
      r.rightWrist.lag = Math.random()*5-3;
      r.rightWrist.ai = Math.random()<0.4?claw_ai: dog_ai;
      r.rightWrist.vigor = 0.05;
      r.rightWrist.sign = 1;
      r.rightWrist.bounce = 2;
      r.rightWrist.bump = grasp;
    }
  },
  56: function() {
    var r = ragdoll.spawn(); /* 8 wounded */
    var ai = claw_ai;
    var loc = ["head","rightWrist","shoulder","leftElbow","rightElbow"];
    for (var i in loc) {
      if (Math.random() < 0.6) {
        r[loc[i]].lag = Math.random()*5-3;
        r[loc[i]].ai = claw_ai;
        r[loc[i]].vigor = 0.1;
        r[loc[i]].sign = -1;
      }
    }
    r.leftWrist.lag = Math.random()*5-3;
    r.leftWrist.ai = claw_ai;
    r.leftWrist.vigor = 0.2;
    r.leftWrist.sign = -1;
  },
  // 57: snake,/* 9 snake */
  57: function() {
    var cable = new Cable();
    for (var i in cable.parts) {
      cable.parts[i].graspable = true;
    }
    cable.parts[0].bump = plug;
    cable.parts[cable.parts.length-1].bump = plug;
  } /* "O" cable */
};

$(document.body).keypress(function(e) {
   if (e.keyCode in spawnKeys) {
     spawnKeys[e.which](e);
   }
});

return {};

});
