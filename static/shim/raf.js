// requestAnimationFrame polyfill by Erik Möller. fixes from Paul Irish and Tino Zijdel
// based off https://gist.github.com/Gaubee/6991570
// modified to use my Performance.now polyfill by Edward Kmett
// MIT license
define("shim/raf",["performance"],function(performance) {
    "use strict"
    var G = window,
        lastTime = 0,
        vendors = ['ms', 'moz', 'webkit', 'o'],
        _KEY_AnimationFrame = 'AnimationFrame',
        _KEY_equest = 'equest',
        _KEY_ancel = 'ancel',
        _KEY_requestAnimationFrame = 'r' + _KEY_equest + _KEY_AnimationFrame,
        _KEY_cancelAnimationFrame = 'c' + _KEY_ancel + _KEY_AnimationFrame,
        now = performance.now;
    for (var x = 0; x < vendors.length && !G[_KEY_requestAnimationFrame]; ++x) {
        G[_KEY_requestAnimationFrame] = G[vendors[x] + 'R' + _KEY_equest + _KEY_AnimationFrame];
        G[_KEY_cancelAnimationFrame] = G[vendors[x] + 'C' + _KEY_ancel + _KEY_AnimationFrame] || G[vendors[x] + 'C' + _KEY_ancel + 'R' + _KEY_equest + _KEY_AnimationFrame];
    }

    // use setInterval?
    if (!G[_KEY_requestAnimationFrame])
        G[_KEY_requestAnimationFrame] = function(callback, element) {
            var currTime = now(),
                timeToCall = Math.max(0, 16 - (currTime - lastTime)),
                id = G.setTimeout(function() { callback(currTime + timeToCall); }, timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };

    if (!G[_KEY_cancelAnimationFrame])
        G[_KEY_cancelAnimationFrame] = function(id) {
            clearTimeout(id);
        };
});
