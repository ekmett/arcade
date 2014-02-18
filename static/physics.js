define(
  ["clip","stats","performance"],
  function(clip,stats,performance) {

var physics = {
  particles   : [],
  constraints : [],
  updated : 0,
  frame   : 0
};

var running = false;

var PRIORITY_PINNED = 0; // forced can move normal things
var PRIORITY_NORMAL = 1; // normal things can move normal things and fluff
var PRIORITY_FLUFF  = 2; // fluff can move fluff
var DEFAULT_ELASTICITY = 0.95;
var FPS = 20;        // frames per second
var MILLISECONDS_PER_FRAME = 1000/FPS;
var RELAXATIONS = 3; // # of successive over-relaxation steps for Gauss-Seidel/Jacobi
var G = 0.2;
var AIR_DRAG = 0.001;
var GROUND_DRAG = 0.2;
var SPEED_LIMIT = 1.5;
var SPEED_LIMIT_SQUARED = SPEED_LIMIT * SPEED_LIMIT;
var RECIP_SPEED_LIMIT_SQUARED = 1 / SPEED_LIMIT_SQUARED
var SPEED_EPSILON = 0.00002;// 0.00001;
var MAX_BODY_HEIGHT = 4; // no Particle is taller than 4 meters z
var MAX_BODY_WIDTH  = 2; // no Particle is wider than 2 meters: x
var MAX_BODY_DEPTH  = 2; // no Particle has a bounding box more than 2 meters deep in y
var BUCKET_WIDTH = MAX_BODY_WIDTH + SPEED_LIMIT; // 4 meters
var BUCKET_DEPTH = MAX_BODY_DEPTH + SPEED_LIMIT;
var BUCKET_COLUMNS = 16; // 96 meters without overlap
var BUCKET_ROWS    = 16; // 96 meters without overlap
var BUCKETS = BUCKET_ROWS * BUCKET_COLUMNS;
var SCENE_WIDTH = 20;
var SCENE_DEPTH = 20;
var SCENE_HEIGHT = 6; // nothing can get more than 5 meters off the ground, making floors about 16ft high.

var buckets = new Array(BUCKETS); // single chained linked lists, how retro

for (var i = 0; i < buckets.length; i++)
  buckets[i] = null; // null, not undefined

function bucket(x,y) {
  return ((Math.floor(x / BUCKET_WIDTH) + BUCKET_COLUMNS) % BUCKET_COLUMNS) + BUCKET_COLUMNS *
         ((Math.floor(y / BUCKET_DEPTH) + BUCKET_ROWS) % BUCKET_ROWS);
}

// basic scene we can replace later with the bsp
var scene = {
  clip : function clip(particle) {
    var nx = Math.max(-SCENE_WIDTH/2,Math.min(particle.x, SCENE_WIDTH/2-particle.w));
    var ny = Math.max(-SCENE_DEPTH/2,Math.min(particle.y, SCENE_DEPTH/2-particle.d));
    var nz = Math.max(0,Math.min(particle.z, SCENE_HEIGHT-particle.h));

    particle.x = nx;
    particle.y = ny;
    particle.z = nz;
  },
  locate : function locate(p) {
    // air friction
    p.mu_h = 0.01;
    p.mu_v = 0.01;
    p.drag_h = 0.01;
    p.drag_v = 0.01;
    p.ground_elasticity = 0;

    var standing = p.standing = p.oz < 0.3; // w/in 1ft of the ground

    if (standing) {
      p.mu_h = 0.35; // standard ground friction is quite high
      p.drag_h = 0.2;
    }
  }
};

var Particle = physics.Particle = function(x,y,z,w,d,h,mass) {
  // primary characterisics
  this.x = x; // position
  this.y = y;
  this.z = z;

  this.ax = this.ay = this.az = 0; // acceleration impulse

  this.ox = x; // retained for both rendering interpolation an verlet integration
  this.oy = y;
  this.oz = z;

  this.oox = x;
  this.ooy = y;
  this.ooz = z;

  this.mass = mass;
  this.inverseMass = 1/mass; // determines collision response

  this.bouncing = false;

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

  this.elasticity = DEFAULT_ELASTICITY;
  this.corrections = 0;

  this.ai = null;

  scene.locate(this); // just so we have local friction information and info about whether we can jump, etc.
};

Particle.prototype = {
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
    this.key = 2*this.rx + 2*this.ry + this.rz + this.w + this.d + this.h*0.51;
  },
  plan: function plan() {
    this.ai && this.ai();
    this.bouncing = false;
    // add gravity , this.beta);;
  },

  move : function move() {

    this.oox = this.ox;
    this.ooy = this.oy;
    this.ooz = this.oz;

    // stash the current location
    var tx = this.x;
    var ty = this.y;
    var tz = this.z;

    var oom = this.inverseMass;

    var vx = (1 - this.mu_h) * (tx - this.ox); // wind, drag
    var vy = (1 - this.mu_h) * (ty - this.oy); // wind, drag
    var vz = (1 - this.mu_v) * (tz - this.oz); // gravity

    // enforce speed floor
    var v2 = vx*vx+vy*vy+vz*vz;

    if (v2 < SPEED_EPSILON && this.standing) {
      vx = 0;
      vy = 0;
      vz = 0;
    }

    vx += this.ax - this.drag_h*vx*vx;
    vy += this.ay - this.drag_h*vy*vy;
    vz += this.az - this.drag_v*vz*vz + (this.lift || -G);

    // enforce speed limit
    v2 = vx*vx+vy*vy+vz*vz;
    if (v2 > SPEED_LIMIT_SQUARED) {
      var s = SPEED_LIMIT / Math.sqrt(v2); // soften this?
      vx *= s;
      vy *= s;
      vz *= s;
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

    this.corrections = 0;
    // thread ourselves onto a bucket based on our new position
    var i = bucket(this.x,this.y);
    this.next_in_bucket = buckets[i];
    buckets[i] = this;
    this.key = 0;
  },
  bump: function(that) {},

  clip2d: clip.clip2d, // these only clip v

  clip3d: clip.clip3d, // these only clip v

  clip_entity : function clip_entity(that) {
    var x1min = this.x;
    var y1min = this.y;
    var z1min = this.z;

    var x1max = x1min + this.w;
    var y1max = y1min + this.d;
    var z1max = z2min + this.h;

    var x2min = that.x;
    var y2min = that.y;
    var z2min = that.z;

    var x2max = x2min + that.w;
    var y2max = y2min + that.d;
    var z2max = z2min + that.h;

    var xo = Math.abs(x1min - x2min);
    var yo = Math.abs(y1min - y2min);
    var zo = Math.abs(z1min - z2min);

    if ( xo < (this.w + that.w)
      && yo < (this.d + that.d)
      && zo < (this.h + that.h) ) {
      // bounding boxes overlap, so we've collided.

      // The player is now a perfectly spherical cow
      var ex = (this.w + that.w)/Math.sqrt(2);
      var ey = (this.d + that.d)/Math.sqrt(2);
      var ez = (this.h + that.h)/Math.sqrt(2);

      var dx = (x1min - x2min + (this.w - that.w)/2) / ex;
      var dy = (y1min - y2min + (this.d - that.d)/2) / ey;
      var dz = (z1min - z2min + (this.h - that.h)/2) / ez;

      var dl = Math.sqrt(dx*dx+dy*dy+dz*dz);

      var l = 1; // Math.sqrt(ex*ex+ey*ey+ez*ez);

      if (dl < l) {
        ++this.corrections;
        ++that.corrections;
        var ima = this.inverseMass;
        var imb = that.inverseMass;
        if (dl * (ima + imb) == 0) {
          return;
          // console.log("singularity approached");
        }
        var E = Math.min(this.elasticity,that.elasticity);
        if (dz < 0) {
          that.bouncing = true;
        } else if (dz > 0) {
          this.bouncing = true;
        }
        var diff = (dl-l)/(dl*(ima+imb))*E;
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

  var ps = physics.particles;
  var cs = physics.constraints;

  if (physics.frame % 25) {
    physics.constraints = physics.constraints.filter(function(e) { return !e.inactive; })
  }

  // figure out local physical properties and plan to get impulses, shoot, etc.
  for (var i in ps) {
    var p = ps[i];
    scene.locate(p); // just so we have local friction information and info about whether we can jump, etc.
    p.plan();
  }

  // unlink the buckets
  for (var i=0;i<buckets.length;i++)
    buckets[i] = null;

  // move and relink the entities
  for (var i in ps)
    ps[i].move();

  // Gauss-Seidel successive relaxation
  for (var k=0;k<RELAXATIONS;k++) {
      // get out of the walls
    for (var i in ps)
      scene.clip(ps[i]);

    // clip all the things
    for (var i = 0; i < buckets.length; i++) {
      clip_bucket(i);
      clip_buckets(i,(i+1) % BUCKETS);
      clip_buckets(i,(i+BUCKET_COLUMNS) % BUCKETS);
      clip_buckets(i,(i+1+BUCKET_COLUMNS) % BUCKETS);
    }

    var str = k / RELAXATIONS;
    // for (var i=cs.length-1;i>=0;--i) cs[i](str);
    for (var i in cs) cs[i](str);
  }

  for (var i in cs)
    if (cs[i].hard) cs[i](str);

  for (var i in ps)
    scene.clip(ps[i]);

  stats.physics.end();
};

var timer = null;

// window.setInterval drifts way too much for a server and client to stay in sync.
function stepper()  {
  var burst = 25; // only catch up a few frames at a time. otherwise controls will get wonky
  var t = performance.now();
  var delay = physics.expected - t;
  while (running && delay < 0 && --burst) { // we're running, we're late, and we're willing to binge
    step(t); // run a frame
    physics.expected += MILLISECONDS_PER_FRAME;
    var t2 = performance.now();
    delay = physics.expected - t2;
    // if (physics.frame % 250 == 0)
    //  console.log("physics frame",physics.frame,"at",(t/1000).toFixed(3),"with delay",(delay/1000).toFixed(3),"took",(t2-t).toFixed(1),"ms");
    t = t2;
  }
  if (running && !burst) {
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
  if (running) {
    // see you next time, same bat time, same bat channel
    timer = window.setTimeout(stepper, delay);
  }
}


// this may have been a bad idea
Object.defineProperties(physics, {
  /* constants */
  FPS                    : { value: FPS,                        __proto__ : null, configurable: false, writable: false, enumerable: true },
  MILLISECONDS_PER_FRAME : { value: MILLISECONDS_PER_FRAME,     __proto__ : null, configurable: false, writable: false, enumerable: true },
  SCENE_WIDTH            : { value: SCENE_WIDTH,                __proto__ : null, configurable: false, writable: false, enumerable: true },
  SCENE_DEPTH            : { value: SCENE_DEPTH,                __proto__ : null, configurable: false, writable: false, enumerable: true },
  SCENE_HEIGHT           : { value: SCENE_HEIGHT,               __proto__ : null, configurable: false, writable: false, enumerable: true },
  RELAXATIONS            : { value: RELAXATIONS,                __proto__ : null, configurable: false, writable: false, enumerable: true },
  G                      : { value: G,                          __proto__ : null, configurable: false, writable: false, enumerable: true },
  SPEED_LIMIT            : { value: SPEED_LIMIT,                __proto__ : null, configurable: false, writable: false, enumerable: true },
  /* read-write attributes */
  scene       : { get: function() { return scene },       set: function(e) { scene = e },       __proto__ : null, configurable: false, enumerable: true },

  running : {
    __proto__ : null,
    configurable: false,
    enumerable: true,
    get: function() { return running; },
    set: function(n) {
      if (n == running) return;
      running = n;
      if (n) {
        physics.expected = performance.now();
        stepper();
      } else if (timer) physics.clearTimeout(timer);
    }
  }
});

// for now just start on launch
physics.running = true;

return physics;

});
