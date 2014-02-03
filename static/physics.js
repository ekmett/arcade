define('physics',
  [],
  function() {
    var physics = {
      fps: 30,
      interval: null
    };

    var start = physics.start = function() {
      if (physics.interval != null)
        window.clearInterval(physics.interval);
      physics.interval = window.setInterval(step,1000/physics.fps);
    };

    var stop = physics.stop = function() {
      if (physics.interval != null)
        winow.clearInterval(physics.interval);
      physics.interval = null;
    };

    return physics;
  }
);
