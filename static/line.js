/*

XKCD-style line interpolation. Roughly based on:
jakevdp.github.com/blog/2012/10/07/xkcd-style-plots-in-matplotlib
http://bl.ocks.org/dfm/3914862

Copyright (c) 2012–2013 Daniel Foreman-Mackey

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

define([], function() {

// Smooth some data with a given window size.
function smooth(d, w) {
  var result = [];
  for (var i = 0, l = d.length; i < l; ++i) {
    var mn = Math.max(0, i - 5 * w),
        mx = Math.min(d.length - 1, i + 5 * w),
        s = 0.0;
    result[i] = 0.0;
    for (var j = mn; j < mx; ++j) {
      var wd = Math.exp(-0.5 * (i - j) * (i - j) / w / w);
      result[i] += wd * d[j];
      s += wd;
    }
    result[i] /= s;
  }
  return result;
}

function xinterp (points) {
  // Scale the data.
  var f = [xscale(xlim[1]) - xscale(xlim[0]), yscale(ylim[1]) - yscale(ylim[0])];
  var z = [xscale(xlim[0]), yscale(ylim[0])];
  var scaled = points.map(function (p) {
    return [(p[0] - z[0]) / f[0], (p[1] - z[1]) / f[1]];
  });

  // Compute the distance along the path using a map-reduce.
  var dists = scaled.map(function (d, i) {
    if (i == 0) return 0.0;
    var dx = d[0] - scaled[i - 1][0],
        dy = d[1] - scaled[i - 1][1];
    return Math.sqrt(dx * dx + dy * dy);
  });
  var dist = dists.reduce(function (curr, d) { return d + curr; }, 0.0);

  // Choose the number of interpolation points based on this distance.
  var N = Math.round(200 * dist);

  // Re-sample the line.
  var resampled = [];

  dists.map(function (d, i) {
    if (i == 0) return;
    var n = Math.max(3, Math.round(d / dist * N));
    var spline = d3.interpolate(scaled[i - 1][1], scaled[i][1]);
    var delta = (scaled[i][0] - scaled[i - 1][0]) / (n - 1);
    for (var j = 0, x = scaled[i - 1][0]; j < n; ++j, x += delta)
      resampled.push([x, spline(j / (n - 1))]);
  });

  // Compute the gradients.
  var gradients = resampled.map(function (a, i, d) {
    if (i == 0) return [d[1][0] - d[0][0], d[1][1] - d[0][1]];
    if (i == resampled.length - 1)
      return [d[i][0] - d[i - 1][0], d[i][1] - d[i - 1][1]];
    return [0.5 * (d[i + 1][0] - d[i - 1][0]),
      0.5 * (d[i + 1][1] - d[i - 1][1])];
  });

  // Normalize the gradient vectors to be unit vectors.
  gradients = gradients.map(function (d) {
    var len = Math.sqrt(d[0] * d[0] + d[1] * d[1]);
    return [ d[0] / len, d[1] / len];
  });

  // Generate some perturbations.
  var perturbations = smooth(resampled.map(d3.random.normal()), 3);

  // Add in the perturbations and re-scale the re-sampled curve.
  var result = resampled.map(function (d, i) {
    var p = perturbations[i], g = gradients[i];
    return [ (d[0] + magnitude * g[1] * p) * f[0] + z[0]
           , (d[1] - magnitude * g[0] * p) * f[1] + z[1]
           ];
  });

  return result; // .join("L");
}

});
