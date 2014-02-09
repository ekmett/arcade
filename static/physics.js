/*
 * Notes:
 *
 * To minimize client side arithmetic, I normalize so the frame rate is the unit
 *
 * To enable easy constraint satisfaction, I'm using basic verlet integration.
 *
 * verlet
 * >>> let fps = 25; a = 9.81/fromIntegral fps^2; step x oldx = (x,oldx) : step (x + x - oldx + a) x in case step 0 (-a/2) !! fps of (x,oldx) -> (oldx,25*(x-oldx-a/2))
 * (4.897152000000011,9.810000000000054)
 *
 * Because javascript is terrible at GC, we're going to keep these alive and mutate in place for speed and memory pressure.
 *
 * This yields a terrible design, but should put little to no allocation pressure on the core physics loop.
 *
 * We're also aggressively inlining and flattening structures as it can make a huge performance difference.
 *
 * Coordinate system:
 *
 * x increases as you go to the right
 * y increases as you go down the screen
 * z increases as you go up
 *
 * This means these do not form an isometric projection in screen space, merely an axonometric one.
 *
 * x and z have unit scale, somewhat artificially
 * y is scaled by a multiplier for display purposes
 *
 * The reason is I can't offload a real isometric projection matrix onto canvas without reshaping all the images as well as their locations
 * in the renderer.
 *
 * Width is associated with x, depth with y, height with z.
 *
 * - Edward Kmett Feb 7, 2014
 */

define(["clip","stats","performance"], function(clip,stats,performance) {

var physics = {
  timer : null,
  frame : 0,
  bodies : [],
  constraints : [],
  running: false
};

var PRIORITY_NORMAL = 0; // normal things can move normal things and fluff
var PRIORITY_FLUFF  = 1; // fluff can move fluff

var FPS = physics.FPS = 40;        // frames per second
var MILLISECONDS_PER_FRAME = physics.MILLISECONDS_PER_FRAME = 1000/FPS;

// stick constraint: a spring that forces the distance between a and b to be l
function stick(a,b,l) {
  return function() {
    var dx = a.x - b.x;
    var dy = a.y - b.y;
    var dz = a.z - b.z;
    var dl = sqrt(dx*dx + dy*dy + dz*dz);
    var ima = a.inverseMass;
    var imb = b.inverseMass;
    var diff = (dl-l)/(dl*(ima+imb))
    dx *= diff;
    dy *= diff;
    dz *= diff;
    a.x -= dx*ima;
    a.y -= dy*ima;
    a.z -= dz*ima;
    b.x += dx*imb;
    b.y += dy*imb;
    b.z += dz*imb;
  }
};

// constants

var RELAXATIONS = 2; // # of successive over-relaxation steps for Gauss-Seidel/Jacobi
var G = -9.8/FPS^2;  // the gravity of the situation

var AIR_DRAG = 0.01;
var GROUND_DRAG = 0.2;

// No Body can move more than 1 meter / frame. This is 25m/s or 56 miles per hour.
// A hard clamp'll serve as a poor man's terminal velocity, but we could switch to a nicer drag model
// to get it more smoothly.
var MAX_VELOCITY    = 1;

var MAX_BODY_HEIGHT = 4; // no Body is taller than 4 meters z
var MAX_BODY_WIDTH  = 2; // no Body is wider than 2 meters: x
var MAX_BODY_DEPTH  = 2; // no Body has a bounding box more than 2 meters deep in y

var MAX_WORLD_HEIGHT = 5; // nothing can get more than 5 meters off the ground, making floors about 16 ft high.
var MIN_WORLD_HEIGHT = 0; // nothing can get more than 0 meters below the floor.

var BUCKET_WIDTH  = MAX_BODY_WIDTH + MAX_VELOCITY*4; // 6 meters
var BUCKET_DEPTH  = MAX_BODY_DEPTH + MAX_VELOCITY*4;

var BUCKET_COLUMNS = 16; // 96 meters without overlap
var BUCKET_ROWS    = 16; // 96 meters without overlap

var BUCKETS = BUCKET_ROWS * BUCKET_COLUMNS;

var buckets = new Array(BUCKETS); // single chained linked lists, how retro
for (var i in buckets) buckets[i] = null; // null, not undefined



function bucket(x,y) {
  return (Math.floor(x / BUCKET_WIDTH) % BUCKET_COLUMNS) + BUCKET_COLUMNS *
         (Math.floor(y / BUCKET_DEPTH) % BUCKET_ROWS);
}

// basic scene we can replace later with the bsp
var scene = {
  clip : function clip(body) {
    // clip the body to the world
    body.z = Math.max(0,Math.min(body.z, MAX_WORLD_HEIGHT-MAX_BODY_HEIGHT));
  },
  locate : function locate(body) {
    // air friction
    body.mu_h = AIR_DRAG;
    body.mu_v = AIR_DRAG;
    body.ground_elasticity = 0; // for bounces

    var standing = body.standing = body.z < 0.3;

    if (standing) {
      body.mu_v = GROUND_DRAG; // standard ground friction
    }
  }
};

var Body = physics.Body = function(x,y,z,w,d,h,inverseMass) {
  // primary characterisics
  this.x = x; // position
  this.y = y;
  this.z = z;

  this.ax = 0 = this.ay = this.az = 0; // acceleration impulse

  this.ox = x; // retained for both rendering interpolation an verlet integration
  this.oy = y;
  this.oz = z;

  this.inverseMass = inverseMass; // determines collision response

  this.w = w; // bounding box parameters
  this.d = d;
  this.h = h;

  this.beta = 1; // assume we made it all the way to the end without clipping.
  this.next_in_bucket = null; // not yet threaded into the world

  // rendering parameters, updated by renderer to reduce display judder, not physics
  this.rx = x;
  this.ry = y;
  this.rz = z;

  this.viewx = this.viewy = this.viewz = 0;

  // bounding box for the current move
  this.minx = x;
  this.miny = y;
  this.minz = z;

  this.maxx = x + w;
  this.maxy = y + d;
  this.maxz = z + h;

  this.priority = PRIORITY_NORMAL;

  this.ai = null;

  scene.locate(this); // just so we have local friction information and info about whether we can jump, etc.
};

Body.prototype = {
  push :  function push(Fx,Fy,Fz) { // apply an instantaneous impulse. mutates in place
    var im = this.inverseMass;
    this.ax += Fx * im;
    this.ay += Fy * im;
    this.az += Fz * im;
  },
  interpolate : function interpolate(alpha) {
    this.rx = this.ox * (1 - alpha) + this.x * alpha;
    this.ry = this.oy * (1 - alpha) + this.y * alpha;
    this.rz = this.oz * (1 - alpha) + this.z * alpha;
  },
  plan: function plan() {
    this.ai && this.ai();
    // add gravity;
  },

  move : function move() {
    // stash the current location
    var tx = this.x;
    var ty = this.y;
    var tz = this.z;

    // probe the bsp to update drag, it'll scribble changes into 'local'

    var oom = this.inverseMass;

    // update position, and derive velocity
    this.x += this.vx = (1 - local.mu_h) * (tx - this.ox) + this.ax; // wind, drag
    this.y += this.vy = (1 - local.mu_h) * (tx - this.ox) + this.ax; // wind, drag
    this.z += this.vz = (1 - local.mu_v) * (tz - this.oz) + this.az + G; // gravity

    // store old position
    this.ox = tx;
    this.oy = ty;
    this.oz = tz;

    // reset impulses
    this.ax = 0;
    this.ay = 0;
    this.az = 0;

    // believe we can go the distance
    this.beta = 1;

    // bounding box for the move
    this.minx = Math.min(this.ox, this.x);
    this.miny = Math.min(this.oy, this.y);
    this.minz = Math.min(this.oz, this.z);

    this.maxx = Math.max(this.ox, this.x) + this.w;
    this.maxy = Math.max(this.oy, this.y) + this.d;
    this.maxz = Math.max(this.oz, this.z) + this.h;

    // thread ourselves onto a bucket based on our new position
    var i = bucket(this.x,this.y);
    this.next_in_bucket = buckets[i];
    buckets[i] = this;
  },

  bump : function bump(that,beta,persistent) {
    if (that.priority <= that.priority) {
      this.beta = Math.min(this.beta, beta);
      // TODO: allow transfer of impulse energy
    }
  },

  clip2d: clip.clip2d, // these only clip v

  clip3d: clip.clip3d, // these only clip v

  // swept aabb collision
  clip_entity : function clip_entity(that) {
    // bounding box for this at start
    var x1min = this.ox;
    var y1min = this.oy;
    var z1min = this.oz;

    var x1max = x1min + this.w;
    var y1max = y1min + this.d;
    var z1max = z2min + this.h;

    // bounding box for that at start
    var x2min = that.ox;
    var y2min = that.oy;
    var z2min = that.oz;

    var x2max = x2min + that.w;
    var y2max = y2min + that.d;
    var z2max = z2min + that.h;

    if ( Math.abs(x1min - x2min) < this.w + that.w
      && Math.abs(y1min - y2min) < this.d + that.d
      && Math.abs(z1min - z2min) < this.h + that.h) {

      // we started overlapping at start of frame

      // TODO: perturb? otherwise we can get stuck together like siamese twins
      this.bump(that,0,true);
      that.bump(this,0,true);
      return true;
    }

    // relative velocity
    var vx = this.vx - that.vx;
    var vy = this.vy - that.vy;
    var vz = this.vz - that.vz;

    var t0 = (x1max < x2min && vx < 0) ? (x1max - x2min) / vx :
             (x2max < x1min && vx > 0) ? (x1min - x2max) / vx : 0

    if (y1max < y2min && vy < 0)      t0 = Math.max(t0, (y1max - y2min) / vy);
    else if (y2max < y1min && vy > 0) t0 = Math.max(t0, (y1min - y2max) / vy);

    if (z1max < z2min && vz < 0)      t0 = Math.max(t0, (z1max - z2min) / vz);
    else if (z2max < z1min && vz > 0) t0 = Math.max(t0, (z1min - z2max) / vz);

    var t1 = 2; // we don't want to register a bump

    if (x2max > x1min && vx < 0)      t1 = Math.min(t1, (x1min - x2max) / vx);
    else if (x1max > x2min && vx > 0) t1 = Math.min(t1, (x1max - x2min) / vx);

    if (y2max > y1min && vy < 0)      t1 = Math.min(t1, (y1min - y2max) / vy);
    else if (y1max > y2min && vy > 0) t1 = Math.min(t1, (y1max - y2min) / vy);

    if (z2max > z1min && vz < 0)      t1 = Math.min(t1, (z1min - z2max) / vz);
    else if (z1max > z2min && vz > 0) t1 = Math.min(t1, (z1max - z2min) / vz);

    if (t0 <= t1 && t1 <= this.beta && t1 <= that.beta) {
      // we have a window of overlap, and it occurs actually during the time we're moving, so bump.
      this.bump(that,t0,false);
      that.bump(this,t0,false);
      return true;
    }
    return false;
  }
};

// O(n^2)
function clip_bucket(i) {
  var a = buckets[i];
  while (a) {
    var b = a.next_in_bucket;
    while (b) {
      a.clip_entity(b);
      b = b.next_in_bucket;
    }
    a = a.next_in_bucket;
  }
}

// O(n*m)
function clip_buckets(i,j) {
  if (buckets[j]) {
    var a = buckets[i];
    while (a) {
      var b = buckets[j];
      while (b) {
        a.clip_entity(b);
        b = b.next_in_bucket;
      }
      a = a.next_in_bucket;
    }
  }
}

var step = function step(t) {
  physics.updated = t;
  ++physics.frame;

  stats.physics.begin();

  var bodies = physics.bodies;
  var buckets = physics.buckets;
  var constraints = physics.constraints;

  // figure out local physical properties and plan to get impulses, shoot, etc.
  for (var i in bodies)
    bodies[i].control();

  // unlink the buckets
  for (var i in buckets)
    buckets[i] = null;

  // move and relink the entities
  for (var i in bodies)
    bodies[i].move();

  // Gauss-Seidel successive relaxation
  for (var r = 0; r < RELAXATIONS; ++r) {
    // update ragdolls and erector sets
    for (var i in constraints)
      constraints[i]();

    // get out of the walls
    for (var i in bodies)
      scene.clip(bodies[i]);

    // clip all the things
    for (var i in buckets) {
      clip_bucket(i);
      clip_buckets(i,(i+1) % BUCKETS);
      clip_buckets(i,(i+BUCKET_COLUMNS) % BUCKETS);
      clip_buckets(i,(i+1+BUCKET_COLUMNS) % BUCKETS);
    }
  }

  stats.physics.end();
};

// window.setInterval drifts way too much for a server and client to stay in sync.
function stepper()  {
  var burst = 25; // only catch up a few frames at a time. otherwise controls will get wonky
  var t = performance.now();
  var delay = physics.expected - t;
  while (physics.running && delay < 0 && --burst) { // we're running, we're late, and we're willing to binge
    step(t); // run a frame
    physics.expected += MILLISECONDS_PER_FRAME;
    var t2 = performance.now();
    delay = physics.expected - t2;
    // if (physics.frame % 250 == 0)
    //  console.log("physics frame",physics.frame,"at",(t/1000).toFixed(3),"with delay",(delay/1000).toFixed(3),"took",(t2-t).toFixed(1),"ms");
    t = t2;
  }
  if (physics.running && !burst) {
     var lag = -delay / MILLISECONDS_PER_FRAME;
     console.warn("physics","lagging", lag.toFixed(1), "frames");
     // if lag gets _too_ high, we should just give up and let the server reset us.
     if (lag > FPS * 60) { // more than a minute
       // TODO: we should ask the server for a big update, we've gone rip van winkle.
       // in case we're connected
       console.warn("physics clock resetting due to lag");
       physics.expected = performance.now();
     }
  }
  if (physics.running) {
    // see you next time, same bat time, same bat channel
    physics.timer = window.setTimeout(stepper, delay);
  }
}

var start = physics.start = function start() {
  physics.running = true;
  physics.expected = performance.now();
  stepper();
};

var stop = physics.stop = function stop() {
  physics.running = false;
  if (physics.timer) physics.clearTimeout(physics.timer);
};

// for now just start on launch
start();

return physics;

});
