define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles", "player","prim","ragdoll"],
  function display(constraints, physics, cc, events, images, transformations, toggles, player, prim, ragdoll) {

var scratch = new transformations.ScreenPoint();
var scratch2 = new transformations.ScreenPoint();
var scratch3 = new transformations.ScreenPoint();

var SCENE_WIDTH  = physics.SCENE_WIDTH;
var SCENE_HEIGHT = physics.SCENE_HEIGHT;
var SCENE_DEPTH  = physics.SCENE_DEPTH;


var ball = function ball(r) {
  var r = r || -Math.log(Math.random())/3+0.2;
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
  particle.draw = function(s,c, alpha) {
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

    // calculate motion vector

    var amp = 0.2;
    var dx1 = (this.x - this.ox)*amp;
    var dy1 = (this.y - this.oy)*amp;
    var dz1 = (this.z - this.oz)*amp;
    var dx2 = (this.ox - this.oox)*amp;
    var dy2 = (this.oy - this.ooy)*amp;
    var dz2 = (this.oz - this.ooz)*amp;
    var dx = dx2 + (1 + alpha) * (dx1 - dx2)
    var dy = dy2 + (1 + alpha) * (dy1 - dy2)
    var dz = dz2 + (1 + alpha) * (dz1 - dz2);
    var d = Math.sqrt(dx*dx + dy*dy + dz*dz)
    scratch.world(this.rx + dx, this.ry + dy, this.rz + dz);
    var x1 = scratch.sx;
    var y1 = scratch.sy;
    scratch.world(this.rx - dx, this.ry - dy, this.rz - dz);
    var x2 = scratch.sx;
    var y2 = scratch.sy;
    var angle = Math.atan2(y2-y1,x2-x1);
    var dw = Math.min(d, this.w/3);

    c.save(); // we translate below
    // draw boring arc
    c.beginPath();
    scratch.world(this.rx,this.ry,this.rz);
    c.translate(scratch.sx,scratch.sy);
    c.rotate(angle);
    c.scale((this.w+dw)/this.w,(this.w-dw)/this.w);
    c.arc(0,0,this.w*Math.sqrt(3),0,2*Math.PI,false);
    c.fillStyle = this.color;
    c.strokeStyle = "#555";
    var dh = this.h*Math.sqrt(2);
    var g = c.createRadialGradient(-Math.sin(angle)*dh,-Math.cos(angle)*dh,0,0,0,this.w*Math.sqrt(3));
    c.globalAlpha = this.opacity || 0.8;
    g.addColorStop(0,"white");
    g.addColorStop(this.specular,this.color); // "black");
    c.fillStyle = g; // this.color; // g;
    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    c.stroke();
    c.fill();
    c.globalAlpha = 1;
    c.restore(); // restore translation

    if (toggles.bounding) {
      prim.cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
      c.lineWidth = 0.01;
      c.strokeStyle = "black";
      c.stroke();
    }

    // draw drop shadow. TODO: rotate this properly
    s.save();
    s.scale(1,0.5);
    s.beginPath();
    scratch.world(this.rx,this.ry,0);
    s.translate(scratch.sx,scratch.sy*2+this.h*2);
    s.arc(0,0,this.w*Math.sqrt(2),0,2*Math.PI,false);
    s.fillStyle = "rgba(0,0,0,0.25)";
    s.fill();
    s.restore();
    c.restore();
  };
  // particle.graspable = true;
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
  var constraints = [];
  for (var i=0;i<l-1;++i) {
    constraints.push(ragdoll.auto(ps[i],ps[i+1]));
  }
  Array.prototype.push.apply(physics.constraints, constraints);

  var snake = this;

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
      var sy = y + ps[i].rz + (ps[i].h + ps[i+1].h)/4 + ps[i+1].rz;
      if (constraints[i-1].inactive == true) {
        c.moveTo(x, y);
        s.moveTo(x, sy)
      } else {
        c.quadraticCurveTo(u.sx, u.sy, x, y);
        s.quadraticCurveTo(u.sx, u.sy + ps[i].rz*2 + ps[i].h/2, x, sy)

        if (snake.legs) {
          c.lineTo(x-snake.legs,y-0.5*snake.legs);
          s.lineTo(x-snake.legs,sy);
          c.moveTo(x, y);
          s.moveTo(x, sy);

          c.lineTo(x+snake.legs,y-0.5*snake.legs);
          s.lineTo(x+snake.legs,sy);
          c.moveTo(x, y);
          s.moveTo(x, sy);
        }
      }
      var t = u; u = v; v = t;
    }
    if (!constraints[n].inactive) {
      v.worldR(ps[n+1]);
      c.quadraticCurveTo(u.sx, u.sy, v.sx, v.sy);
      s.quadraticCurveTo(u.sx, u.sy + (ps[n].rz*2 + ps[n].h/2), v.sx, v.sy + ps[n+1].rz*2 + ps[n+1].h/2);
    }

    c.lineCap = 'round';
    c.lineJoin = 'round';
    c.lineWidth = r+0.05;
    c.strokeStyle = "#000";
    c.stroke();
    c.lineWidth = r-0.05;
    c.strokeStyle = color;
    c.stroke();
    c.lineWidth = 0.05; // offset upwards?
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
  this.constraints = constraints;

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
  var c = new Cable(8);
  c.legs = 0.1;
  var r = c.radius;
  var ps = c.parts;

  var lag = Math.random()*2;
  ps[0].ai = snake_ai;
  ps[0].bounce = r*r*6;
  ps[0].lag = lag + 4;
  ps[0].vigor = 0.6;
  ps[0].vscale = 1;
  ps[0].intendedX = 0;
  ps[0].intendedY = 0;
  ps[0].intendedZ = 0;
  ps[0].bump = grasp;
  ps[0].grip = 2.4;
  ps[0].releaseFrequency = 0.9;

  ps[3].ai = snake_ai;
  ps[3].bounce = r*r*2;
  ps[3].lag = lag + 6;
  ps[3].vscale = 1;
  ps[3].vigor = 0.2;
  ps[3].intendedX = 0;
  ps[3].intendedY = 0;
  ps[3].intendedZ = 0;

  ps[ps.length-1].bump = plug;
};

var dog_ai = function dog_ai() {
  // if (Math.random() >= this.vigor) return;
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var dz = player.oz * this.lag + player.z * (1 - this.lag) - this.z;
  var l = Math.sqrt(dx*dx+dy*dy+dz*dz);

  if (this.grasping && (physics.frame % 12 == 0) && (Math.random() < (this.releaseFrequency || 0.3))) {
    this.grasping.inactive = true;
    --this.grasping.onto.grasped;
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

  if (this.grasping && (physics.frame % 12 == 0) && (Math.random() < (this.releaseFrequency || 0.3))) {
    this.grasping.inactive = true;
    --this.grasping.onto.grasped;
    this.grasping = null;
  }

  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    if (this.standing) this.push((Math.random()*dx-0.05)*(this.vscale||2),(Math.random()*dy-0.05)*(this.vscale||2),Math.random());
  }
};

var plug = function plug(that) {
  if (!this.plugged && that.outlets > 0 && that.acceptsPlug(this)) {
    this.plugged = true;
    that.outlets--;
    var l = Math.min(that.w,that.d,that.h)/2;
    var c = constraints.stick(this,that,l);
    physics.constraints.push(c);
  }
}

var grasp = function grasp(that) {
  if (this.grasping && this.grasping.inactive) this.grasping = undefined;
  if (!this.grasping && that.graspable == true && Math.random() < (this.graspFrequency || 0.6)) {
    var l = Math.min(that.w,that.d,that.h);
    this.grasping = constraints.stick(this,that,l*0.3,4*(this.grip || 2));
    this.grasping.oz = -Math.random()*(that.graspRange || (that.h/3))-(that.graspTop || (that.h/3));
    this.grasping.onto = that;
    this.grasping.hard = true;
    that.grasped = (that.grasped || 0) + 1;
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
    b.vscale = 4;
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
    var b = ball(2); // +Math.random()*0.4);
    physics.particles.push(b);
    b.color = "#0ff";
    var speed1 = Math.random()*0.1-0.05;
    var speed2 = Math.random()*0.1-0.05;
    b.opacity = 0.8;
    b.start = Math.random()*40;
    // b.graspable = true;
    b.outlets = 6; // Math.floor(particle.w/0.3);
    b.acceptsPlug = function(that) {
      return that.tag === "cable";
    };
    b.ai = function() {
      var dx = Math.sin((b.start+physics.frame)*3.14/20)*speed1;
      var dy = Math.cos((b.start+physics.frame)*3.14/20)*speed2;
      this.push(0,0,100); // dx*2,dy*2,10*this.w*this.d); // lift proportional to surface area.
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
    var originalSize = b.w;
    var originalMass = b.mass;
    b.bump = function(that) {
      if (!that.constrained) {
        this.constraints++;
        // this.w = this.h = this.d = originalSize / this.constraints;
        // this.mass = originalMass / (this.constraints^3)
        // this.inverseMass = 1/ this.mass;
        that.constrained = true;
        var l = Math.min(this.w+that.w,this.d+that.d,this.h+that.h)/2;
        physics.constraints.push(constraints.stick(b,that,l*0.9,10));
      }
      grasp.call(this,that); // in addition, grasp graspables.
    }
    b.grip = 2.5;
  },
  54: function() { /* 6 zombie */
    var r = ragdoll.spawn();
    var loc = ["shoulder","waist","leftElbow","rightElbow"];
    for (var i in loc) {
      if (Math.random() < 0.2) {
        r[loc[i]].lag = Math.random()*5-3;
        r[loc[i]].ai = dog_ai;
        r[loc[i]].vigor = 0.05;
        r[loc[i]].bounce = 2.4;
        r[loc[i]].vscale = 0.2;
      }
    }
    r.head.bump = grasp;
    r.head.grip = 2.45;
    r.head.lag = Math.random()*5-3;
    r.head.ai = dog_ai;
    r.head.vigor = 0.05;
    r.head.bounce = 3;
    r.head.vscale = 0.5;

    r.leftWrist.lag = Math.random()*5-3;
    r.leftWrist.ai = claw_ai; // Math.random()<0.4?claw_ai: dog_ai;
    r.leftWrist.vigor = 0.05;
    r.leftWrist.sign = 1;
    r.leftWrist.bump = function(that) {
      grasp.call(this,that);
      plug.call(this,that);
    };
    r.leftWrist.vscale = 2;
    r.leftWrist.grip = 2.3;
    r.leftWrist.tag = "wrist";
    r.rightWrist.lag = Math.random()*5-3;
    r.rightWrist.ai = dog_ai; // Math.random()<0.4?claw_ai: dog_ai;
    r.rightWrist.vscale = 1.2;
    r.rightWrist.vigor = 0.05;
    r.rightWrist.sign = 1;
    r.rightWrist.bounce = 2;
    r.rightWrist.bump = function(that) {
      grasp.call(this,that);
      plug.call(this,that);
    };
    r.rightWrist.grip = 2.3;
    r.rightWrist.tag = "wrist";
    r.leftAnkle.tag = "ankle";
    r.leftAnkle.bump = plug;
    r.rightAnkle.tag = "ankle";
    r.rightAnkle.bump = plug;
  },
  // 55: snake,/* 7 snake */
  56: function() { /* 8 cable */
    var cable = new Cable();
    //  for (var i in cable.parts) cable.parts[i].graspable = true;
    cable.parts[0].bump = plug;
    cable.parts[0].tag = "cable";
    cable.parts[cable.parts.length-1].bump = plug;
    cable.parts[cable.parts.length-1].tag = "cable";
    cable.legs = 0.1;
  },
  57: function() { /* 9 spider */
    var r = 0.16;
    var b = new physics.Particle(
      Math.random()*(SCENE_WIDTH-r)-SCENE_WIDTH/2,
      Math.random()*(SCENE_DEPTH-r)-SCENE_DEPTH/2,
      Math.random()*(SCENE_HEIGHT-r),
      r,r,r,10
    );
    //b.color = "#356";
    b.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
    b.bump = grasp;
    b.graspRange = 0.9;
    b.graspTop = 0.1;
    b.grip = 1.83;
    physics.particles.push(b);

    var pick = function(x,y) {
      scratch.world(this.rx,this.ry,this.rz);
      var dx = scratch.sx-x;
      var dy = scratch.sy-y;
      var near = dx*dx + dy*dy < this.w * this.w * 3;
      return near ? this.key : null;
    };

    b.pick = pick;

    var box = function(s,c) {
      if (toggles.bounding) {
        prim.cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
        c.lineWidth = 0.01;
        c.strokeStyle = "#000";
        c.stroke();
      }
    };


    var Leg = function(dx,dy,bump) {
      this.toe = new physics.Particle(b.x+dx,b.y+dy,b.z-0.1,0.05,0.05,0.05,2);
      this.toe.draw = box;
      this.toe.elasicity = 0.01;
      this.toe.pick = pick;
      // if (bump) this.toe.bump = bump;
      this.toe.bump = grasp;
      this.toe.graspFrequency = 0.1;
      this.toe.graspRange = 0.9;
      this.toe.graspTop = 0.1;
      this.leg = ragdoll.auto(b,this.toe);
      physics.particles.push(this.toe);
      physics.constraints.push(this.leg);
    };

    // var s = Math.sqrt(2);
    var legs = [
      new Leg(0.3,0,grasp),    new Leg(0,0.3,grasp),
      new Leg(-0.3,0,null),   new Leg(0,-0.3,null)
      // new Leg(0.2*s,0.2*s,true),  new Leg(0.2*s,-0.2*s,true),
      // new Leg(-0.3/s,0.3/s, null)
    ];

    b.elasticity = 0.01;
    b.outlets = 1;
    b.acceptsPlug = function(that) {
      return that.tag === "wrist" || that.tag === "cable" || that.tag === "ankle";
    };
    b.handy = true;
    b.ai = function() {
      if (this.grasping && this.grasping.inactive) this.grasping = null;

      if ((this.standing || this.bouncing) && !this.grasping && Math.random() < 0.6) {
        var dx = player.x - b.x;
        var dy = player.y - b.y;
        var d = Math.sqrt(dx*dx+dy*dy);
        dx /= d;
        dy /= d;
        var impulse = 0;
        for (var i in legs) {
          var lx = legs[i].toe.x - b.x;
          var ly = legs[i].toe.y - b.y;
          var l = Math.sqrt(lx*lx+ly*ly);
          lx /= l;
          ly /= l;

          var dot = Math.sqrt(dx*lx+dy*ly);

          if (Math.random() < 0.72) {
            if (dot > 0) {
              legs[i].toe.push(dx,dy,Math.random());
            } else {
              impulse += Math.random()*4;
              legs[i].toe.push(dx-0.1*dy,dy+0.1*dx,Math.random());
            }
          }
        }
       b.push(impulse*dx,impulse*dy,Math.random()*5);
      }
    };

    b.draw = function(s,c,alpha) {
      c.save();
      s.save();

      scratch.worldR(this);
      var x = scratch.sx;
      var y = scratch.sy;
      var z = this.rz;

      c.beginPath();
      s.beginPath();
      s.lineWidth = 0.1;
      for (var i in legs) {
        c.moveTo(x,y);
        scratch.worldR(legs[i].toe);
        c.quadraticCurveTo(scratch.sx,scratch.sy-0.4,scratch.sx,scratch.sy);
        c.lineTo(scratch.sx,scratch.sy);
        s.moveTo(x,y+z*2);
        s.lineTo(scratch.sx,scratch.sy+legs[i].toe.rz*2);


        // unless we're a hand
        if (b.outlets) {
          c.moveTo(x,y);
          s.moveTo(x,y+z*2);
          var xl = x + x - scratch.sx
          var yl = scratch.sy;
          c.quadraticCurveTo(xl,yl-0.4,xl,yl);
          s.lineTo(x + x - scratch.sx,scratch.sy+legs[i].toe.rz*2);
          c.lineTo(xl,yl);
        }
      }
      c.lineCap = 'round';
      c.lineBevel = 'round';
      c.lineWidth = 0.2;
      c.strokeStyle = '#000';
      c.stroke();
      c.lineWidth = 0.1;
      c.strokeStyle = this.color;
      c.stroke();
      s.lineWidth = 0.1;
      s.strokeStyle = 'rgba(0,0,0,0.1)';
      s.stroke();

      s.scale(1,0.5);
      s.beginPath();
      c.beginPath();
      c.arc(x,y,r*Math.sqrt(3),0,2*Math.PI,false);
      s.arc(x,y*2+z*4,r*Math.sqrt(3),0,2*Math.PI,false);
      c.fillStyle = this.color;
      c.lineWidth = 0.1;
      c.strokeStyle = '#000';
      c.stroke();
      c.fill();
      s.fillStyle = 'rgba(0,0,0,0.2)'
      s.fill();

      c.restore();
      s.restore();
    };
  }
};

$(document.body).keypress(function(e) {
   if (e.keyCode in spawnKeys) {
     spawnKeys[e.which](e);
   }
});

return {};

});
