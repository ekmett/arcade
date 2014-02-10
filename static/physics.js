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

var FPS = physics.FPS = 25;        // frames per second
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
    var diff = (dl-l)/(dl*(ima+imb));
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

var RELAXATIONS = 1; // # of successive over-relaxation steps for Gauss-Seidel/Jacobi
var G = 0.1; // 9.8/FPS^2*0; // 0.1; // 9.8/FPS^2 / 100;  // the gravity of the situation

var AIR_DRAG = 0.001;
var GROUND_DRAG = 0.2;

// No Body can move more than 1 meter / frame. This is 25m/s or 56 miles per hour.
// A hard clamp'll serve as a poor man's terminal velocity, but we could switch to a nicer drag model
// to get it more smoothly.
var SPEED_LIMIT    = 1;
var SPEED_LIMIT_SQUARED = SPEED_LIMIT * SPEED_LIMIT;
var RECIP_SPEED_LIMIT_SQUARED = 1 / SPEED_LIMIT_SQUARED
var SPEED_EPSILON = 0;// 0.00001;

var MAX_BODY_HEIGHT = 4; // no Body is taller than 4 meters z
var MAX_BODY_WIDTH  = 2; // no Body is wider than 2 meters: x
var MAX_BODY_DEPTH  = 2; // no Body has a bounding box more than 2 meters deep in y

var MAX_WORLD_HEIGHT = 8; // nothing can get more than 5 meters off the ground, making floors about 16 ft high.
var MIN_WORLD_HEIGHT = 0; // nothing can get more than 0 meters below the floor.

var BUCKET_WIDTH  = MAX_BODY_WIDTH + SPEED_LIMIT*2; // 4 meters
var BUCKET_DEPTH  = MAX_BODY_DEPTH + SPEED_LIMIT*2;

var BUCKET_COLUMNS = 16; // 96 meters without overlap
var BUCKET_ROWS    = 16; // 96 meters without overlap

var BUCKETS = BUCKET_ROWS * BUCKET_COLUMNS;

var buckets = new Array(BUCKETS); // single chained linked lists, how retro

for (var i = 0; i < buckets.length; i++)
  buckets[i] = null; // null, not undefined

function bucket(x,y) {
  return ((Math.floor(x / BUCKET_WIDTH) + BUCKET_COLUMNS) % BUCKET_COLUMNS) + BUCKET_COLUMNS *
         ((Math.floor(y / BUCKET_DEPTH) + BUCKET_ROWS) % BUCKET_ROWS);
}

// basic scene we can replace later with the bsp
var scene = {
  clip : function clip(body) {
    var nx = Math.max(-5,Math.min(body.x, 5-body.w));
    var ny = Math.max(-5,Math.min(body.y, 5-body.d));
    var nz = Math.max(0,Math.min(body.z, MAX_WORLD_HEIGHT-body.h));

    body.x = nx;
    body.y = ny;
    body.z = nz;
  },
  locate : function locate(body) {
    // air friction
    body.mu_h = 0.01;
    body.mu_v = 0.01;
    body.ground_elasticity = 0;

    var standing = body.standing = body.oz < 0.3; // w/in 1ft of the ground

    if (standing) {
      body.mu_v = 0.9; // standard ground friction is quite high
    }
  }
};

var Body = physics.Body = function(x,y,z,w,d,h,mass) {
  // primary characterisics
  this.x = x; // position
  this.y = y;
  this.z = z;

  this.ax = this.ay = this.az = 0; // acceleration impulse

  this.ox = x; // retained for both rendering interpolation an verlet integration
  this.oy = y;
  this.oz = z;

  this.mass = mass;
  this.inverseMass = 1/mass; // determines collision response

  this.w = w; // bounding box parameters
  this.d = d;
  this.h = h;

  this.next_in_bucket = null; // not yet threaded into the world

  // rendering parameters, updated by renderer to reduce display judder, not physics
  this.rx = x;
  this.ry = y;
  this.rz = z;

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
    alpha = Math.min(alpha);
    this.rx = this.ox * (1 - alpha) + this.x * alpha;
    this.ry = this.oy * (1 - alpha) + this.y * alpha;
    this.rz = this.oz * (1 - alpha) + this.z * alpha;
    this.key = this.rx + this.ry + 2*this.rz + (this.w + this.h + this.d)/2;
  },
  plan: function plan() {
    this.ai && this.ai();
    // add gravity , this.beta);;
  },

  move : function move() {

    // stash the current location
    var tx = this.x;
    var ty = this.y;
    var tz = this.z;

    var oom = this.inverseMass;

    var vx = (1 - this.mu_h) * (tx - this.ox); // wind, drag
    var vy = (1 - this.mu_h) * (ty - this.oy); // wind, drag
    var vz = (1 - this.mu_v) * (tz - this.oz); // gravity

    // enforce speed floor
    var v2 = vx*vx+vy*vy;

    if (v2 < SPEED_EPSILON && this.standing) {
      vx = 0;
      vy = 0;
    }

    vx += this.ax;
    vy += this.ay;
    vz += this.az - G;

    // enforce speed limit, lest we clip through things
    v2 = vx*vx+vy*vy+vz*vz;

    if (v2 > SPEED_LIMIT_SQUARED) {
      var v = Math.sqrt(v2);
      // console.log("speeding detected",Math.sqrt(v2)); // this.x,this.y,this.z,vx,vy,vz);
      vx /= v;
      vy /= v;
      vz /= v;
    }

    // update position, and derive new velocity
    this.x += vx;
    this.y += vy;
    this.z += vz;

    // store old position
    this.ox = tx;
    this.oy = ty;
    this.oz = tz;

    // reset impulses
    this.ax = 0;
    this.ay = 0;
    this.az = 0;

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
    this.key = 0;
  },
  bump: function(that) {},

  clip2d: clip.clip2d, // these only clip v

  clip3d: clip.clip3d, // these only clip v

  // swept aabb collision
  clip_entity : function clip_entity(that) {
    var x1min = this.x;
    var y1min = this.y;
    var z1min = this.z;

    var x1max = x1min + this.w;
    var y1max = y1min + this.d;
    var z1max = z2min + this.h;

    // bounding box for that at start
    var x2min = that.x;
    var y2min = that.y;
    var z2min = that.z;

    var x2max = x2min + that.w;
    var y2max = y2min + that.d;
    var z2max = z2min + that.h;

    var xo = Math.abs(x1min - x2min);
    var yo = Math.abs(y1min - y2min);
    var zo = Math.abs(z1min - z2min);

    if ( xo < this.w + that.w
      && yo < this.d + that.d
      && zo < this.h + that.h) {
      // bounding boxes overlap, so we've collided.


      // The player is now a pefectly spherical cow
      var ex = (this.w + that.w); //2;
      var ey = (this.d + that.d); //2;
      var ez = (this.h + that.h); //2;

      var dx = (x1min - x2min + (this.w - that.w)/2) / ex;
      var dy = (y1min - y2min + (this.d - that.d)/2) / ey;
      var dz = (z1min - z2min + (this.h - that.h)/2) / ez;

      var dl = Math.sqrt(dx*dx+dy*dy+dz*dz);

      var l = 1; // Math.sqrt(ex*ex+ey*ey+ez*ez);

      if (dl < l) {
        if (dl * (ima + imb) > 200) {
          console.log("singularity approached");
        }
        var ima = this.inverseMass;
        var imb = that.inverseMass;
        var diff = (dl-l)/(dl*(ima+imb))*0.4; // a magical elasticity coefficient
        dx *= diff*ex;
        dy *= diff*ey;
        dz *= diff*ez;
        this.x -= dx*ima;
        this.y -= dy*ima;
        this.z -= dz*ima;
        that.x += dx*imb;
        that.y += dy*imb;
        that.z += dz*imb;
        this.bump(that);
        that.bump(this);
      }
    }
  }
};

// O(n^2)
function clip_bucket(i) {
  // if (i % 256 == 0) console.log("clipping",i);
  var a = buckets[i];
  while (a != null) {
    var b = a.next_in_bucket;
    while (b != null) {
      a.clip_entity(b);
      b = b.next_in_bucket;
    }
    a = a.next_in_bucket;
  }
}

// O(n*m)
function clip_buckets(i,j) {
  var a = buckets[i];
  while (a != null) {
    var b = buckets[j];
    while (b != null) {
      a.clip_entity(b);
      b = b.next_in_bucket;
    }
    a = a.next_in_bucket;
  }
}

var step = function step(t) {
  physics.updated = t;
  ++physics.frame;

  stats.physics.begin();

  var bodies = physics.bodies;
  var constraints = physics.constraints;

  // figure out local physical properties and plan to get impulses, shoot, etc.
  for (var i in bodies) {
    var b = bodies[i];
    scene.locate(b); // just so we have local friction information and info about whether we can jump, etc.
    b.plan();
  }

  for (var k=0;k<1;k++) {

  // unlink the buckets
  for (var i=0;i<buckets.length;i++)
    buckets[i] = null;

  // move and relink the entities
  for (var i in bodies)
    bodies[i].move();

  // Gauss-Seidel successive relaxation
  for (var i in constraints)
    constraints[i]();

    // get out of the walls
  for (var i in bodies)
    scene.clip(bodies[i]);

    // clip all the things
  for (var i = 0; i < buckets.length; i++) {
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
