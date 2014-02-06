// requirejs friendly polyfill for Performance.now. doesn't edit the global namespace
define([],function() {
  var perf = window.Performance || {};

  if (!perf.now) {
    var then = (perf.timing && perf.timing.navigationStart) ? perf.timing.navigationStart : Date.now();
    perf.now = function() {
      return Date.now() - then;
    };
  }
  return perf;
});
