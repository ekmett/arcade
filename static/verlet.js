define(["physics/pool"], function (pool) {

var SPEED_LIMIT_SQUARED = 1; // most stuff should be below this.
var LOG_BUCKET_WIDTH = 1;
var LOG_BUCKET_DEPTH = 1;
var FLAG_BIG = 1;
var RELAXATIONS = 2;

var V3Handler     = new pool.Handler(3,Float32Array);
var FlagHandler   = new pool.Handler(1,Uint8Array);
var BucketHandler = new pool.Handler(1,Int32Array);

var Bodies = pool.Pool.extend(function (base) { return {
  init: function Bodies_init(N) {
    base.init.call(this,N);
    this.has("position",        V3Handler)      // position
        .has("velocity",        V3Handler)      // velocity
        .has("acceleration",    V3Handler)      // half acceleration
        .has("oldAcceleration", V3Handler)      // half old acceleration
        .has("size",            V3Handler)
        .has("flags",           FlagHandler);   // physics flags
        .has("next_in_bucket",  BucketHandler); // intrusive linked list
    this.buckets = new Int32Array(257); // 16*16 + 1 'big/fast' bucket
  },
  /* Clip a pair of bodies */
  clip_bodies: function Bodies_clip_bodies(i,j) {
    // TODO: midphase and nearphase collision of two objects if they can interact


  },
  /* Clip everything in a bucket against itself */
  clip_bucket: function Bodies_clip_bucket(i) {
    i = this.buckets[bi]
    while (i >= 0) {
      var j = this.next_in_bucket[i];
      while (j >= 0) {
        clip_bodies(i,j);
        j = this.next_in_bucket[j];
      }
      i = this.next_in_bucket[i];
    }
  },
  clip_buckets: function Bodies_clip_buckets(i,k) {
    i = this.buckets[i];
    while (i>=0) {
      var j = this.buckets[k];
      while (j>=0) {
        clip_bodies(i,j);
        j = this.next_in_bucket[j]l
      }
      i = this.next_in_bucket[i]
    }
  }
  /* filter collisions */
  clip_local: function Bodies_clip_local() {
    // now loop trough buckets and clip everything against everything.
    for (var i=0;i<256;++i) {
      this.clip_buckets(i,256); // clip against big/fast stuff
      this.clip_bucket(i);      // clip locally
      this.clip_buckets(i,(i+1) &255);
      this.clip_buckets(i,(i+16)&255);
      this.clip_buckets(i,(i+17)&255);
    }
  },
  step: function() {
    var buckets = this.buckets;
    for (var i=0;i<buckets.length;++i)
      buckets[i] = -1;

    // velocity verlet
    var p  = this.position;
    var v  = this.velocity;
    var a  = this.acceleration;
    var oa = this.oldAcceleration;
    var inactive = this.inactive;
    var inactivity = 0;
    for (var i=0,l=this.length;i<l;++i) {
      // check if we're alive
      if (!(i&31))
        inactivity = inactive[w++];
      var dead = inactivity & 1;
      inactivity >>= 1;
      if (dead) continue;

      var flags = this.flags[i];

      // we should also skip updating sleeping items eventually
      var i3 = i*3;

      var oax = oa[i3]
      var x  = p[i3] += v[i3] + oax;
      var vx = v[i3] += oax + a[i3];

      var oay = oa[i3+1];
      var y  = p[i3+1] += v[i3+1] + oay;
      var vy = v[i3+1] += oay + a[i3+1];

      var oaz = oa[i3+2];
      var z  = p[i3+2] += v[i3+2] + oaz;
      var vz = v[i3+2] += oaz + a[i3+2];

      // broadphase filtering is handled by a repeating grid with a sin bin for fast/big objects
      // if we ever add a significant z component we can factor that in here
      var bucket = (flags & FLAGS_BIG || vx*vx+vy*vy >= SPEED_LIMIT_SQUARED) ? 256
                 : ((x>>LOG_BUCKET_WIDTH) & 15) + 16 * ((y>>LOG_BUCKET_DEPTH) & 15);
      next_in_bucket[i] = this.buckets[bucket];
      this.buckets[bucket] = i;
    }

    this.oldAcceleration = a;
    this.acceleration = new Float32Array(this.N);

    for (var r=0;r<RELAXATIONS;++r) {
      this.clip_scene();
      this.clip_local();
      // TODO: enforce constraints
    }
    this.clip_scene();
  }
}});

return {
  V3: V3
  Verlet: Verlet
}

}); // define
