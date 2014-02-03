define('physics',
  [],
  function() {
    var physics = {
      fps: 30,
      interval: null
      actors: {
        active: []
        passive: []
      }
    };

    var Actor = function (id,x,y) {
      this.id = id;
      this.x = x;
      this.y = y;
      this.dx = x
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
