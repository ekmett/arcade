define(["aabb","bsp","clip","shim/perf"], function(aabb,bsp,clip,perf) {

// coordinate system:
//
// x increases as you go to the right
// y increases as you go down the screen
// z increases as you go up
//
// This means these do not form an isometric basis, merely an axonometric one.
//
// x and z have unit scale, somewhat artificially
// y is scaled by a multiplier for display purposes

var id = 0;

var physics = {
  timer      : null,
  bodies     : [],
  constraints: [],
};

var frame = 0;      // current frame #

var RELAXATIONS = 2; // # of successive over-relaxation steps for Gauss-Seidel/Jacobi
var FPS = 25;       // frames per second
var G = -9.8/FPS^2; // the gravity of the situation

// No Body can move more than 1 meter / frame. This is 25m/s or 56 miles per hour.
// A hard clamp'll serve as a poor man's terminal velocity, but we could switch to a nicer drag model
// to get it more smoothly.
var MAX_VELOCITY    = 1;

var MAX_BODY_HEIGHT = 4; // no Body is taller than 4 meters z
var MAX_BODY_WIDTH  = 2; // no Body is wider than 2 meters: x
var MAX_BODY_DEPTH  = 2; // no Body is mor than 2 meters deep y

var MAX_WORLD_HEIGHT = 5; // nothing can get more than 5 meters off the ground, making floors about 16 ft high.
var MIN_WORLD_HEIGHT = 0; // nothing can get more than 0 meters below the floor.

var BUCKET_WIDTH  = MAX_BODY_WIDTH + MAX_VELOCITY*2; // 4 meters
var BUCKET_DEPTH  = MAX_BODY_DEPTH + MAX_VELOCITY*2;

var BUCKET_COLUMNS = 16;
var BUCKET_ROWS    = 16;

var BUCKETS = BUCKET_ROWS * BUCKET_COLUMNS;

var buckets = new Array(BUCKETS); // single chained linked lists, how retro

for (var i in buckets) buckets[i] = null; // null, not undefined

function bucket(x,y) {
  return (Math.floor(x / BUCKET_WIDTH) % BUCKET_COLUMNS) + BUCKET_COLUMNS * (Math.floor(y / BUCKET_DEPTH) % BUCKET_ROWS)
}

var lerp = function(x,y,alpha) {
  return x * (1 - alpha) + y * alpha;
};

// to minimize client side arithmetic, we normalize so the frame rate is the unit
//
// verlet
// >>> let fps = 25; a = 9.81/fromIntegral fps^2; step x oldx = (x,oldx) : step (x + x - oldx + a) x in case step 0 (-a/2) !! fps of (x,oldx) -> (oldx,25*(x-oldx-a/2))
// (4.897152000000011,9.810000000000054)

// because javascript is terrible at GC, we're going to keep these alive and mutate in place for speed and memory pressure.
var Body = physics.Body = function(x,y,z,w,d,h,inverseMass) {
  // primary characterisics
  this.x = x; // position
  this.y = y;
  this.z = z;

  this.ax = 0; // acceleration impulse
  this.ay = 0;
  this.az = 0;

  this.ox = x; // retained for both rendering interpolation an verlet integration
  this.oy = y;
  this.oz = z;

  this.id = ++id; // client-local unique id

  this.inverseMass = inverseMass; // determines collision response

  this.w = w; // bounding box parameters
  this.d = d;
  this.h = h;

  this.beta = 1; // assume we made it all the way to the end without clipping.
  this.next_in_bucket = null; // not yet threaded into the world
};

Body.prototype = {
  impulse :  function(Fx,Fy,Fz) { // apply an instantaneous impulse. mutates in place
    var im = this.inverseMass;
    this.ax += Fx * im;
    this.ay += Fy * im;
    this.az += Fz * im;
  },
  interpolate : function(alpha) {
    return { x: lerp(this.ox, this.x, alpha),
           , y: lerp(this.oy, this.y, alpha),
           , z: lerp(this.oz, this.z, alpha),
           };
  },
  move : function() {
    // stash the current location
    var tx = this.x;
    var ty = this.y;
    var tz = this.z;

    // probe the bsp to calculate drag and gravity here
    var local = bsp.local(this.x,this.y,this.z);

    // update position to be the goal
    this.x = tx + (1 - local.horizontal_drag) * tx - this.ox + this.ax + local.ax; // wind, drag
    this.y = ty + (1 - local.horizontal_drag) * ty - this.oy + this.ay + local.ay;
    this.z = tz + (1 - local.vertical_drag) * tz - this.oz + this.az + local.az; // gravity

    // store old position
    this.ox = tx;
    this.oy = ty;
    this.oz = tz;

    // reset impulses
    this.ax = 0;
    this.ay = 0;
    this.az = 0;

    // believe we'll move the whole distance
    this.beta = 1;

    // thread ourselves onto a hash bucket.
    var i = bucket(this.x,this.y);
    this.next_in_bucket = buckets[i];
    buckets[i] = this;
  },
};

var step = function() {
  physics.updated = perf.now();
  ++frame;

  // unlink the buckets
  for (var i in buckets)
    buckets[i] = null;

  for (var i in entities) {
    entities[i].move();
  }

  for (var i in constraints) {
    constraints[i].apply();
  }

    var bounce = 0;

    // surfaces are perfect shock absorbers for now.

    var leftover = clip(move);
    while (clips < max_clips && energy) {
      // once clipped, we go more or less Euler.
      var x1 = move.x1 = move.x2;
      var y1 = move.y1 = move.y2;
      var z1 = move.z1 = move.z2;
      leftover.x
      move.x2 = x1 + leftover.vx;
      move.y2 = y1 + leftover.vy;
      move.z2 = z1 + leftover.vz;
    }

    for (var j in brushes) {
    }

    for (var j in entities) {
      if (j != i) {
      }
    }

    var c = clipping(it, it.ox, it.oy, it.oz, it.x, it.y, it.z, 0);
    var plane = bsp.clip(c);
    if (plane) {
    }
    if (bsp.clip(c)) {
      // we're good, take the new position as gospel
    } else {
      // 

    }

    
    if (bsp.clip(it)) {
      bsp.track(it);
    }

    var clipping = new Clipping(aabb(it.ox,it.oy,it.oz,it.etc.w,it.etc.h,it.etc.d),it.x-it.ox,it.y-it.oy,it.z-it.oz);
    // go go gadget O(n^2) algorithm

    var path = 
    entities[i].vv();

  }
  for (var i in entities
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
