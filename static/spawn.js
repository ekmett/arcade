define(
  ["constraints", "physics", "shim/cc", "events", "images", "transformations", "toggles", "player","prim","ragdoll"],
  function display(constraints, physics, cc, events, images, transformations, toggles, player, prim, ragdoll) {

var scratch = new transformations.ScreenPoint();
var scratch2 = new transformations.ScreenPoint();

function curveThrough(s, c, points) {
  var n = points.length - 2;
  var u = scratch, v = scratch2;
  u.worldR(points[1]);
  for (var i=1;i<n;i++) {
    v.worldR(points[i+1]);
    var x = (u.sx + v.sx) * 0.5;
    var y = (u.sy + v.sy) * 0.5;
    c.quadraticCurveTo(u.sx, u.sy, x, y);
    s.quadraticCurveTo(u.sx, u.sy + points[i].rz*2 + points[i].h/2, x, y + points[i].rz + (points[i].h + points[i+1].h)/4 + points[i+1].rz);
    var t = u; u = v; v = t;
  }
  v.worldR(points[n+1]);
  c.quadraticCurveTo(u.sx, u.sy, v.sx, v.sy);
  s.quadraticCurveTo(u.sx, u.sy + (points[n].rz*2 + points[n].h/2), v.sx, v.sy + points[n+1].rz*2 + points[n+1].h/2);
}

var SCENE_WIDTH  = physics.SCENE_WIDTH;
var SCENE_HEIGHT = physics.SCENE_HEIGHT;
var SCENE_DEPTH  = physics.SCENE_DEPTH;


var ball = function ball() {
  var r = -Math.log(Math.random())/3+0.2;
  var particle = new physics.Particle(Math.random()*(SCENE_WIDTH-r)-SCENE_WIDTH/2,Math.random()*(SCENE_DEPTH-r)-SCENE_DEPTH/2, Math.random()*(SCENE_HEIGHT-r),r,r,r,40*r^2.8);
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
    s.arc(scratch.sx,scratch.sy*2+this.h*2,this.w*Math.sqrt(2),0,2*Math.PI,false);
    s.fillStyle = "rgba(0,0,0,0.25)";
    s.fill();
    s.restore();
    c.restore();
  };
  return particle;
};

var dog_ai = function dog_ai() {
  // if (Math.random() >= this.vigor) return;
  var dx = player.ox * this.lag + player.x * (1 - this.lag) - this.x;
  var dy = player.oy * this.lag + player.y * (1 - this.lag) - this.y;
  var dz = player.oz * this.lag + player.z * (1 - this.lag) - this.z;
  var l = Math.sqrt(dx*dx+dy*dy+dz*dz);
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
  if (Math.abs(l) > 0.01) {
    dx /= l;
    dy /= l;
    if (this.standing) this.push(dx*2,dy*2,Math.random());
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
    if (Math.random() < 0.5) {
      r.rightWrist.lag = Math.random()*5-3;
      r.rightWrist.ai = Math.random()<0.4?claw_ai: dog_ai;
      r.rightWrist.vigor = 0.05;
      r.rightWrist.sign = 1;
      r.rightWrist.bounce = 2;
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
  57: function() { /* 9 */
    var r = Math.random()*0.1+0.25;
    var l = Math.floor(Math.random()*10+6);
    var ps = new Array(l);
    for (var i=0;i<l;i++) {
      ps[i] = new physics.Particle(0,0.5*i,0,r,r,r,23.5*r^2);
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
      ps[i].pick = function(x,y) {
        scratch.world(this.rx,this.ry,this.rz);
        var dx = scratch.sx-x;
        var dy = scratch.sy-y;
        var near = dx*dx + dy*dy < this.w * this.w * 3;
        return near ? this.key : null;
      };
      physics.particles.push(ps[i]);
    }

    var oldDraw = ps[0].draw;
    ps[0].draw = function(s,c) {
      c.save();
      s.save();

      c.beginPath();
      s.beginPath();
      scratch.world(this.rx,this.ry,this.rz);
      c.moveTo(scratch.sx,scratch.sy);
      s.moveTo(scratch.sx,scratch.sy+this.rz*2);

      curveThrough(s,c,ps);
      c.lineCap = 'round';
      c.lineWidth = r+0.05;
      c.strokeStyle = "#000";
      c.stroke();
      c.lineWidth = r-0.05;
      c.strokeStyle = "#eee";
      c.stroke();

      s.lineCap = 'round';
      s.lineWidth = r;
      s.strokeStyle = "rgba(0,0,0,0.25)";
      s.stroke();

      oldDraw.call(this,s,c);
      c.restore();
      s.restore();
    };
    ps[0].ai = dog_ai;
    ps[0].bounce = 1.5;
    ps[0].lag = 0.5;
    ps[0].vscale = 1.75;
    ps[2].ai = dog_ai;
    ps[2].bounce = 4.5;
    ps[2].lag = 0;
    ps[2].vscale = 1.75;
    for (var i=1;i<l;++i) {
      ps[i].pred = ps[i-1];
    }
    var base = physics.frame;
    for (var i=3;i<l/2;++i) {
      ps[i].ai = function() {
      };
    }
    for (var i=0;i<l-1;++i) {
      physics.constraints.push(ragdoll.auto(ps[i],ps[i+1]));
    }
  }
};

$(document.body).keypress(function(e) {
   if (e.keyCode in spawnKeys) {
     spawnKeys[e.which](e);
   }
});

return {};

});
