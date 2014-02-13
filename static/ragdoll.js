define(
  ["physics","transformations","constraints", "toggles","prim"],
  function(physics, transformations, constraints, toggles, prim) {

var Particle = physics.Particle;

var ragdoll = {};

function auto(a,b,c) {
  var dx = a.x - b.x;
  var dy = a.y - b.y;
  var dz = a.z - b.z;
  var result = constraints.stick(a,b,Math.sqrt(dx*dx+dy*dy+dz*dz));
  result.a = a;
  result.b = b;
  result.c = c;
  return result;
}

var scratch = new transformations.ScreenPoint();

// 2 units tall
var Ragdoll = function Ragdoll (w,d,h,m) {
  var r = 0.1;
  this.head        = new Particle( 0,      0.1*d,  0.95*h,  0.35,0.35,0.45,5.1*m)
  this.shoulder    = new Particle( 0,      0,      0.85*h,  0.2,0.2,0.2, 28.08*m);
  this.leftElbow   = new Particle( 0.2*w,  0,      0.67*h, 0.15,0.15,0.3, 3.7*m);
  this.rightElbow  = new Particle(-0.2*w,  0,      0.67*h, 0.15,0.15,0.3, 3.7*m);
  this.leftWrist   = new Particle( 0.3*w,  0.1*d,  0.42*h, 0.05,0.05,0.05, 2.25*m);
  this.rightWrist  = new Particle(-0.3*w,  0.1*d,  0.42*h, 0.05,0.05,0.05, 2.25*m);
  this.waist       = new Particle( 0,     -0.1*d,  0.60*h, 0.3,0.3,0.3, 33.06*m);
  this.pelvis      = new Particle( 0,      0,      0.52*h, 0.2,0.2,0.2, 13.66*m);
  this.leftKnee    = new Particle( 0.2*w,  0.2*d , 0.28*h, 0.1,0.1,0.3, 8*m);
  this.rightKnee   = new Particle(-0.2*w,  0.2*d , 0.28*h, 0.1,0.1,0.3, 8*m);
  this.leftAnkle   = new Particle( 0.1*w, -0.05*d, 0.05*h, 0.15,0.15,0.3, 8*m);
  this.rightAnkle  = new Particle(-0.1*w, -0.05*d, 0.05*h, 0.15,0.15,0.3, 8*m);

  var constraints = this.constraints = [
    auto(this.head,this.shoulder),
    auto(this.shoulder,this.leftElbow),
    auto(this.shoulder,this.rightElbow),
    auto(this.leftElbow,this.leftWrist),
    auto(this.rightElbow,this.rightWrist),
    auto(this.shoulder,this.waist),
    auto(this.waist,this.pelvis),
    auto(this.shoulder,this.pelvis,false),
    auto(this.pelvis,this.head,false),
    auto(this.pelvis,this.leftKnee),
    auto(this.pelvis,this.rightKnee),
    auto(this.leftKnee,this.leftAnkle),
    auto(this.rightKnee,this.rightAnkle)
  ];
  var parts = this.parts = [this.head,this.shoulder,this.leftElbow,this.rightElbow,this.leftWrist,this.rightWrist,this.waist,this.pelvis,this.leftKnee,this.rightKnee, this.leftAnkle,this.rightAnkle];

  for (var i in this.parts) {
    physics.particles.push(this.parts[i]);
    this.parts[i].draw = function(s,c) {
      prim.cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
      c.lineWidth = 0.01;
      c.strokeStyle = "#000";
      c.stroke();
    };
  }

  for (var i in this.constraints)
    physics.constraints.push(this.constraints[i]);

  var oldDraw = this.head.draw
  this.head.draw = function(s,c) {
    s.save();
    c.save();
    s.strokeStyle = 'rgba(0,0,0,0.3)';
    s.lineWidth = 0.1;

    if (toggles.soft_shadows) {
      c.shadowBlur = this.w;
      c.shadowColor = 'rgba(0,0,0,0.8)';
      c.shadowOffsetX = 0; // 8*this.w;
      c.shadowOffsetY = 0; // 4*this.w;
    }

    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    for (i in constraints) {
      var it = constraints[i];
      if (typeof it.c !== 'undefined') continue;
      c.beginPath();
      s.beginPath();
      scratch.world(it.a.rx, it.a.ry, it.a.rz);
      c.moveTo(scratch.sx, scratch.sy);
      s.moveTo(scratch.sx, scratch.sy+it.a.rz*2);
      scratch.world(it.b.rx, it.b.ry, it.b.rz);
      c.lineTo(scratch.sx, scratch.sy);
      s.lineTo(scratch.sx, scratch.sy+it.b.rz*2);
      c.stroke();
      s.stroke();
    }
    c.beginPath();
    scratch.world(this.rx,this.ry,this.rz);
    c.arc(scratch.sx,scratch.sy-this.h,this.w*Math.sqrt(3),0,2*Math.PI,false);
    c.fillStyle = "#fff"
    c.lineWidth = 0.15;
    c.strokeStyle = "black";
    c.stroke();
    c.fill();

    s.beginPath();
    s.scale(1,0.5);
    s.arc(scratch.sx,scratch.sy*2+this.rz*4,this.w*Math.sqrt(3),0,2*Math.PI,false);
    s.fillStyle = 'rgba(0,0,0,0.2)';
    s.fill();

    c.restore();
    s.restore();
  };
};

var spawn = ragdoll.spawn = function() {
  var result = new Ragdoll(0.5,0.5,2,1);
  var fx = Math.random()*3000-1500;
  var fy = Math.random()*3000-1500;
  var fz = Math.random()*3000-500;
  for (var i in result.parts) {
    result.parts[i].push(fx,fy,fz);
  }
  return result;
};

return ragdoll;

});
