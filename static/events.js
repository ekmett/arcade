define("events",["jquery"],function($) {

var map = $('#glass');

var events = {
  impulse: {}, // keys held
  mouse  : {}, // mouse buttons held
  over   : false,
  actions: [], // events for when these things happened
  mouseX : 0,  // last known position
  mouseY : 0,
  log    : function() {}// function() { console.log.apply(console,arguments); } // function() {} // replace with console.log to watch
};

var stahp = function(e) {
  if (e) {
    if (e.preventDefault) e.preventDefault();
    if (e.stopPropagation) e.stopPropagation();
  }
  return false;
};

$(".map").each(function (i,it) {
   it.ondragstart   = stahp;
   it.onselectstart = stahp;
});

document.body.ontouchstart = stahp;
document.body.ontouchmove = stahp;

$(document.body).keydown(function (e) {
  e.preventDefault();
  // TODO: check if the focused element is absorbing these events
  events.log("events","keydown",e.which);
  events.impulse[e.which] = e.timeStamp;
  events.actions.push(e);
  return false;
});

$(document.body).keyup(function (e) {
  e.preventDefault();
  events.log("events","keyup",e.which);
  delete events.impulse[e.which];
  events.actions.push(e);
  return false;
});

map.mousemove(function(e) {
  e.preventDefault();
  events.log("events","mousemove",e.pageX,e.pageY,e.offsetX, e.offsetY);
  events.mouseX = e.offsetX;
  events.mouseY = e.offsetY;
  return false;
});

map.on({'touchstart' : function (e) {
  e = e.originalEvent || e;
  e.preventDefault();
  var touch = e.touches[0];
  events.log("events","touchstart",e.pageX,e.pageY,e.offsetX, e.offsetY);
  events.mouseX = e.offsetX;
  events.mouseY = e.offsetY;
  return false;
}});

map.on({'touchmove' : function (e) {
  e = e.originalEvent || e;
  e.preventDefault();
  var touch = e.touches[0];
  events.log("events","touchmove",e.pageX,e.pageY,e.offsetX, e.offsetY);
  events.mouseX = e.offsetX;
  events.mouseY = e.offsetY;
  return false;
}});

map.mousedown(function(e) {
  e.preventDefault();
  events.log("events","mousedown",e.which);
  events.mouse[e.which] = e.timeStamp;
  events.actions.push(e);
  return false;
});

map.mouseup(function(e) {
  e.preventDefault();
  events.log("events","mouseup",e.which);
  delete events.mouse[e.which]
  events.actions.push(e);
  return false;
});

map.mouseover(function(e) {
  e.preventDefault();
  events.log("events","mouseover");
  events.over = true;
  events.actions.push(e);
  return false;
});

map.mouseout(function(e) {
  e.preventDefault()
  events.log("events","mouseout");
  events.over = false;
  events.actions.push(e);
  return false;
});

return events;

});
