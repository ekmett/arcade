define(["aabb","bsp","shim/perf"], function(aabb,bsp,perf) {

var id = 0;

var physics = {
  timer: null,
  bodies: []
};

var fps = 25;

var G = 9.8;

var lerp = physics.lerp = function(x,y,alpha) {
  return x * (1 - alpha) + y * alpha;
};

var Body = physics.Body = function(x,y,z,px,py,pz,mass,inverseMass) {
  // primary characterisics
  this.x = x;   // center of mass
  this.y = y;
  this.z = z;
  this.px = px; // momentum
  this.py = py;
  this.pz = pz;
  // constants
  this.mass   = mass;
  this.inverseMass = inverseMass = inverseMass || 1/mass;
  // update secondary characterisics
  this.vx = px * inverseMass; // velocity
  this.vy = py * inverseMass;
  this.vz = pz * inverseMass;
};

Body.prototype = {
  copy : function() {
    return new Body(
      this.x, this.y, this.z,
      this.px, this.py, this.pz,
      this.mass, this.inverseMass
    );
  },
  lerp : function(that, alpha) {
    return new Body(
      lerp(this.x,  that.x,  alpha),
      lerp(this.y,  that.y,  alpha),
      lerp(this.z,  that.z,  alpha),
      lerp(this.px, that.px, alpha),
      lerp(this.py, that.py, alpha),
      lerp(this.pz, that.pz, alpha),
      lerp(this.mass, that.mass, alpha)
    );
  },
  euler: function(t, dt, der) {
    der = der || this.now(t);
    return new Body(
      this.x + a.vx*dt,
      this.y + a.vy*dt,
      this.z + a.vz*dt,
      this.px + a.Fx*dt,
      this.py + a.Fy*dt,
      this.pz + a.Fz*dt,
      this.mass,
      this.inverseMass
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
      this.px + 1/6 * dt * (a.Fx + 2*(b.Fx + c.Fx) + d.Fx),
      this.py + 1/6 * dt * (a.Fy + 2*(b.Fy + c.Fy) + d.Fy),
      this.pz + 1/6 * dt * (a.Fz + 2*(b.Fz + c.Fz) + d.Fz),
      this.mass,
      this.inverseMass
    );
  },
  now: function(t) {
    var mu = -0.01; // simple Stokes' drag, add ground drag, etc.
    return { vx : this.vx
           , vy : this.vy
           , vz : this.vz
           , Fx : mass * mu * this.vx
           , Fy : mass * mu * this.vy
           , Fz : mass * (G + mu * this.vz)
           };
  },
  then: function(t, dt, der) {
    return this.euler(t,dt,der).now(t + dt)
  }
};

var step = function() {
  physics.updated = perf.now();
  // update everything

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
