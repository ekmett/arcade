define(["aabb","bsp","shim/perf"], function(aabb,bsp,perf) {

var id = 0;

var physics = {
  timer: null,
  entities: []
};

var frame = 0;

var fps = 25;

var G = 9.8;

var lerp = physics.lerp = function(x,y,alpha) {
  return x * (1 - alpha) + y * alpha;
};

// other stuff
var Etc = physics.Etc = function(mass,w,h,d) {
  this.mass = mass;
  this.inverseMass = 1/mass;
  this.w = w;
  this.h = h;
  this.d = d;
};

Etc.prototype = {
  // we don't interpolate w, h, d, mass
  lerp: function(that, alpha) { return this; }
};

var Body = physics.Body = function(x,y,z,vx,vy,vz,etc) {
  // primary characterisics
  this.x = x; // position
  this.y = y;
  this.z = z;
  this.vx = vx; // velocity
  this.vy = vy;
  this.vz = vz;
  // constants
  this.etc = etc;
  var oom = etc.inverseMass;
  // secondary characterisics
};

Body.prototype = {
  copy : function() {
    return new Body(
      this.x, this.y, this.z,
      this.vx, this.vy, this.vz,
      this.etc
    );
  },
  impulse : function(Fx,Fy,Fz) { // apply an instantaneous impulse
    var oom = this.etc.inverseMass;
    return new Body(
      this.x, this.y, this.z,
      this.vx + Fx * oom, this.vy + Fy * oom, this.vz + Fz * oom,
      etc
    );
  },
  lerp : function(that, alpha) {
    return new Body(
      lerp(this.x,  that.x,  alpha),
      lerp(this.y,  that.y,  alpha),
      lerp(this.z,  that.z,  alpha),
      lerp(this.vx, that.vx, alpha),
      lerp(this.vy, that.vy, alpha),
      lerp(this.vz, that.vz, alpha),
      that.etc
    );
  },
  euler: function(t, dt, der) {
    der = der || this.now(t);
    return new Body(
      this.x + a.vx*dt,
      this.y + a.vy*dt,
      this.z + a.vz*dt,
      this.vx + a.ax*dt,
      this.vy + a.ay*dt,
      this.vz + a.az*dt,
      this.etc
    );
  },
  // switch to velocity verlet for easier constraints?
  rk4 : function(t, dt) { // dt = 1 frame
    var a      = this.now(t);
    var halfdt = 0.5*dt;
    var b      = this.then(t,halfdt,a);
    var c      = this.then(t,halfdt,b);
    var d      = this.then(t,dt,c);
    return new Body(
      this.x + 1/6 * dt * a.vx + 2*(b.vx + c.vx) + d.vx,
      this.y + 1/6 * dt * a.vy + 2*(b.vy + c.vy) + d.vy,
      this.z + 1/6 * dt * a.vz + 2*(b.vz + c.vz) + d.vz,
      this.vx + 1/6 * dt * (a.ax + 2*(b.ax + c.ax) + d.ax),
      this.vy + 1/6 * dt * (a.ay + 2*(b.ay + c.ay) + d.ay),
      this.vz + 1/6 * dt * (a.az + 2*(b.az + c.az) + d.az),
      this.etc
    );
  },
  now: function(t) {
    var mu = -0.01; // simple Stokes' drag, add ground drag, etc.
    return { vx : this.vx
           , vy : this.vy
           , vz : this.vz
           , ax : mu * this.vx
           , ay : mu * this.vy
           , az : G + mu * this.vz
           };
  },
  // invoked with varying dt's
  then: function(t, dt, der) {
    return this.euler(t,dt,der).now(t + dt)
  }
};

var Entity = physics.Entity = function(then, now) {
  this.then = null;
  this.now = now;
  this.future = null;
};

Entity.prototype = {
  interpolate : function (alpha) {
    return this.then ? this.then.lerp(this.now, alpha) : this.now;
  }
};

var step = function() {
  physics.updated = perf.now();
  ++frame;
  // update everything, mixes old universe and new universe state, rather than properly bisecting, but is fast
  for (var i in entities) {
    var old = entities[i].then = entities[i].now;
    entities[i].now = old.euler(frame,1);
  }
};

var start = physics.start = function() {
  if (physics.timer != null)
    window.clearInterval(physics.timer);
  physics.interval = window.setInterval(step,1000/physics.fps);
};

var stop = physics.stop = function() {
  if (physics.interval != null)
    window.clearInterval(physics.interval);
  physics.interval = null;
};

return physics;

});
