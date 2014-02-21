define(["fiber"], function(fiber) {

// A custom pool based allocator for structure-of-arrays style pools using typed arrays
// with explicit compaction
// Edward Kmett 2014

var BadAlloc = fiber.extend(function() { return {
  toString : function BadAlloc_toString() {
    return "Attempted to allocate a negative number of particles"
  }
}}); // BadAlloc

var Handler = fiber.extend(function() { return {
  init : function(stride, array) {
    this.stride = stride;
    this.array  = array; // the typed array class involved
  }
}});

// takes an initial guess at a number of particles to start with
// if it is to small you'll thrash a bit as you start to allocate.
// but we grow on a doubling cube as needed
var Pool = fiber.extend(function() { return {
  init: function Pool_init(N) {
    N = N || 4;
    this.N = N;
    this.length = 0;
    this.inactive = new Uint32Array((N+31)>>5);
  },
  handlers: {},
  alloc : function Pool_alloc(k) {
    if (typeof k === 'undefined') k = 1; // default allocation is 1 item
    var l = this.length; // current length
    var lk = l + k;      // updated length
    if (lk <= this.N) {
      // we fit, give back a contiguous run of k items starting at l
      if (k < 0) throw new BadAlloc();
      this.length = lk;
      return l;
    } else {
      // we don't fit, find a new max size
      var M = this.N;
      while (lk > M) M*=2;

      var handlers = this.handlers;
      // copy the contents
      for (var h in handlers) {
        var handler = handlers[h];
        var ins  = this[h];
        var outs = new handler.array(M*handler.stride);
        outs.set(l == this.N ? ins : ins.subarray(0,l*handler.stride));
        this[h] = outs;
      }
      // grow the inactive subarray as needed
      var oldInactive = this.inactive;
      var newInactive = this.inactive = new Uint32Array((M+31)>>5)
      var K = (l+31)>>5;
      newInactive.set(K == oldInactive.length ? oldInactive : oldInactive.subarray(0,K));
      this.length = lk;
      this.N = M;
      return l; // retun a contiguous run of k particles starting at l
    }
  },
  has : function (name, handler) {
    this.handlers[name] = handler;
    this[name] = new handler.array(handler.stride*this.N);
    return this;
  },
  free : function Pool_free(i) {
    this.inactive[i>>5] |= 1<<(i&31);
  },
  // in place compaction:
  //
  // if anything was compacted, it returns a bit vector
  // indicating the compacted positions, otherwise it returns null
  compact : function Pool_compact() {
    var l = this.length;
    var K = (l+31)>>5;
    var i = 0;
    var inactive = this.inactive;
    // stride 32 at a time until we find an inactive item
    while (i<K&&!inactive[i]) ++i;
    i *= 32;
    var j = i;
    if (i >= l) return null; // we made a clean sweep
    var handlers = this.handlers;
    for (;i<l;++i) {
      // TODO: jump around and use set and subarray in here for a lot of speed.
      if (!(inactive[i>>5]&(1<<(i&31)))) {
        for (var h in handlers) {
          var array = this[h];
          var handler = handlers[h];
          var s = handler.stride;
          var is = i*s;
          var js = j*s;
          for (k=0;k<s;++k)
            array[js+k] = array[is+k];
        }
        ++j;
      }
    }
    if (i == j) return null; // we made a clean sweep regardless
    this.length = j;
    this.inactive = new Uint32Array(inactive.length);
    return inactive; // sanity ceck
  },
}}); // ParticleSystem

var V3 = new Handler(3,Float32Array);

var ParticleSystem = Pool.extend(function (base) { return {
  init: function() {
    base.init.call(this);
    this.has("pos",V3)
        .has("oldpos",V3)
        .has("accel",V3);
  },
  // Perform a Verlet integration timestep for all particles
  step: function() {
    var pos    = this.pos;
    var oldpos = this.oldpos;
    var accel  = this.accel;
    // handles all 3 dimensions
    for (var i=0,l=this.length*3;i<l;++i) {
      var x = pos[i];
      // update new position and set acceleration
      oldpos[i] = x + (x - oldpos[i]) + accel[i];
      accel[i] = 0;
    }
    // swap position vectors
    this.oldpos = pos;
    this.pos = oldpos;
  }
}});

return {
  BadAlloc       : BadAlloc,
  Pool           : Pool,
  V3             : V3,
  ParticleSystem : ParticleSystem
};

}); // define
