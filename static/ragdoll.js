define(
  ["physics","transformations","constraints"],
  function(physics, transformations, constraints) {

var Particle = physics.Particle;

var ragdoll = {};

function auto(a,b) {
  var dx = a.x - b.x;
  var dy = a.y - b.y;
  var dz = a.z - b.z;
  var result = constraints.stick(a,b,Math.sqrt(dx*dx+dy*dy+dz*dz));
  result.a = a;
  result.b = b;
  return result;
}

var scratch = new transformations.ScreenPoint();

// 2 units tall
var Ragdoll = function Ragdoll (w,d,h,m) {
  var r = 0.1;
  this.head        = new Particle( 0,      0.1*d,  0.9*h,  0.25,0.25,0.35,5.1*m)
  this.shoulder    = new Particle( 0,      0,      0.8*h,  r,r,r, 28.08*m);
  this.leftElbow   = new Particle( 0.2*w,  0,      0.62*h, r,r,r, 3.7*m);
  this.rightElbow  = new Particle(-0.2*w,  0,      0.62*h, r,r,r, 3.7*m);
  this.leftWrist   = new Particle( 0.3*w,  0.1*d,  0.42*h, r,r,r, 0.65*m);
  this.rightWrist  = new Particle(-0.3*w,  0.1*d,  0.42*h, r,r,r, 0.65*m);
  this.waist       = new Particle( 0,     -0.1*d,  0.55*h, r,r,r, 13.06*m);
  this.pelvis      = new Particle( 0,      0,      0.45*h, r,r,r, 13.66*m);
  this.leftKnee    = new Particle( 0.2*w,  0.2*d , 0.25*h, r,r,r, 8*m);
  this.rightKnee   = new Particle(-0.2*w,  0.2*d , 0.25*h, r,r,r, 8*m);
  this.leftAnkle   = new Particle( 0.1*w, -0.05*d, 0.05*h, r,r,r, 8*m);
  this.rightAnkle  = new Particle(-0.1*w, -0.05*d, 0.05*h, r,r,r, 8*m);

  var constraints = this.constraints = [
    auto(this.head,this.shoulder),
    auto(this.shoulder,this.leftElbow),
    auto(this.shoulder,this.rightElbow),
    auto(this.leftElbow,this.leftWrist),
    auto(this.rightElbow,this.rightWrist),
    auto(this.shoulder,this.waist),
    auto(this.waist,this.pelvis),
    auto(this.pelvis,this.leftKnee),
    auto(this.pelvis,this.rightKnee),
    auto(this.leftKnee,this.leftAnkle),
    auto(this.rightKnee,this.rightAnkle)
  ];
  var parts = this.parts = [this.head,this.shoulder,this.leftElbow,this.rightElbow,this.leftWrist,this.rightWrist,this.waist,this.pelvis,this.leftKnee,this.rightKnee, this.leftAnkle,this.rightAnkle];

  for (var i in this.parts) {
    physics.particles.push(this.parts[i]);
    this.parts[i].draw = function(s,c) {};
  }

  for (var i in this.constraints)
    physics.constraints.push(this.constraints[i]);

  var oldDraw = this.head.draw
  this.head.draw = function(s,c) {
    c.save();
    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    for (i in constraints) {
      var it = constraints[i];
      c.beginPath();
      scratch.world(it.a.rx, it.a.ry, it.a.rz);
      c.moveTo(scratch.sx, scratch.sy);
      scratch.world(it.b.rx, it.b.ry, it.b.rz);
      c.lineTo(scratch.sx, scratch.sy);
      c.stroke();
    }
    c.beginPath();
    scratch.world(this.rx,this.ry,this.rz);
    c.arc(scratch.sx,scratch.sy,this.w*Math.sqrt(3),0,2*Math.PI,false);
    c.fillStyle = "#fff"
    c.lineWidth = 0.1;
    c.strokeStyle = "black";
    c.stroke();
    c.fill();
    c.restore();
  };
};

var spawn = ragdoll.spawn = function() {
  var result = new Ragdoll(0.5,0.5,2,100);
  var fx = Math.random()*3000-1500;
  var fy = Math.random()*3000-1500;
  var fz = Math.random()*3000-1500;
  for (var i in result.parts) {
    result.parts[i].push(fx,fy,fz);
  }
};

return ragdoll;

});
