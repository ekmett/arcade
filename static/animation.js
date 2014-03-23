define([], function() {
// common animation and easing functions

// Perlin bias function
function bias(a,b) {
  return Math.pow(b,Math.log(a)/Math.log(0.5));
};

// symmetric first order approximation of bias
function fastBias(a,b) {
  return b/((1/a-2)*(1-b)+1);
};

// Perlin's gain
function gain(a,b) {
  if (b < 0.5) return bias(1-a,2*b)/2;
  else return 1 - bias(1-a,2-2*b)/2;
};

// piecewise first order approximaton of gain
function fastGain(a,b) {
  if (b < 0.5) return fastBias(1-a,2*b)/2;
  else return 1 - fastBias(1-a,2-2*b)/2;
};

function smoothstep(a,b,t) {
  if (t < a) return 0;
  if (t >= b) return 1;
  if (a == b) return -1;
  var p = (t - a) / (b - a);
  return (p * p * (3 - 2 * p));
};

function clamp(t,a,b) {
  if (t < a) return a;
  if (t > b) return b;
  return t;
}

function lerp(t,a,b) {
  return a + t*(b-a);
}

function pulse(a,b,t) {
  return (a <= t) && (t <= b) ? 1 : 0;
}

function step(a,t) {
  return (t >= a) ? 1 : 0;
}

return {
  bias: bias,
  gain: gain,
  fastBias: fastBias,
  fastGain: fastGain,
  smoothstep: smoothstep,
  clamp: clamp,
  lerp: lerp,
  pulse: pulse,
  step: step
};

});
