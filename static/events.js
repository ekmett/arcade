define("events",["jquery"],function($) {
  var map = $('#foreground');

  var events = {
    impulse: {}, // keys held
    mouse  : {}, // mouse buttons held
    over   : false,
    actions: [], // events for when these things happened
    mouseX : 0,  // last known position
    mouseY : 0
  };

  $(document).keydown(function (e) {
    // TODO: check if the focused element is absorbing these events
    // console.log("events","keydown",e.which);
    events.impulse[e.which] = e.timeStamp;
    events.actions.push(e);
  });

  $(document).keyup(function (e) {
    // TODO: check if the focused element is absorbing these events
    // console.log("events","keyup",e.which);
    delete events.impulse[e.which];
    events.actions.push(e);
  });

  map.mousemove(function(e) {
    console.log("events","mousemove",e.pageX,e.pageY,e.offsetX, e.offsetY);
    events.mouseX = e.offsetX;
    events.mouseY = e.offsetY;
  });

  map.mousedown(function(e) {
    // console.log("events","mousedown",e.which);
    events.mouse[e.which] = e.timeStamp;
    events.actions.push(e);
  });

  map.mouseup(function(e) {
    // console.log("events","mouseup",e.which);
    delete events.mouse[e.which]
    events.actions.push(e);
  });

  map.mouseover(function(e) {
    // console.log("events","mouseover");
    events.over = true;
    events.actions.push(e);
  });

  map.mouseout(function(e) {
    // console.log("events","mouseout");
    events.over = false;
    events.actions.push(e);
  });

  return events;
});
