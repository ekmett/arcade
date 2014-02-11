define(["jquery", "constraints", "physics", "shim/raf", "shim/cc", "performance", "stats", "events","images"], function display($, constraints, physics, raf, cc, performance, stats, events, images) {

var display = {
  updated : null,
  frame : 0
};

var main = $("#main");

var scrollX = 0;
var scrollY = 0;

var width   = 800; // set during resize
var height  = 400; // set during resize

// this lets us see '25 meters x 25 meters' in world space in a 2:1 dimetric projection

var halfWidth = 800;
var halfHeight = 400;

var PIXELS_PER_METER = 20;
var METERS_PER_PIXEL = 1 / PIXELS_PER_METER;

var Y_SCALE  = 0.5;
var RECIP_Y_SCALE = 2;

var layer = function layer(name) {
  var result = $("#" + name);
  result.canvas = result[0].getContext("2d");
  return result;
};

var background = layer("background");
var shadows    = layer("shadows");
var foreground = layer("foreground");

var layers = display.layers = [ background, shadows, foreground ];

var playarea = $(".playarea");

var c = foreground.canvas; // most things draw here, give it a short name

// to avoid javascript object allocation we stuff transformed coordinates in one of these.
var ScreenPoint = function ScreenPoint (sx,sy) {
  this.sx = +sx;
  this.sy = +sy;
};

// embed from world coordinates: translate, transform, translate. the matrix isn't any simpler operation count-wise.
ScreenPoint.prototype.world = function world(x,y,z) {
  x -= scrollX; // offset into the world
  y -= scrollY;
  this.sx = 2*(y-x);
  this.sy = x+y-2*z;
};

var WorldPoint = function WorldPoint (x,y) {
  this.x = +x;
  this.y = +y;
};

WorldPoint.prototype.screen = function screen(sx,sy) {
  this.x = scrollX - 0.25*sx + 0.5*sy;
  this.y = scrollY + 0.25*sx + 0.5*sy;
};

var resized = function resized() {
  width  = Math.min(document.body.clientWidth, 800);
  height = Math.min((document.body.clientHeight) - foreground.offset().left, 400);
  halfWidth = width / 2;
  halfHeight = height / 2;
  playarea.each(function(i,e) {
    $(e).width(width);
    $(e).height(height);
  });
  console.log("play area resized to",width,height);
};

$(window).bind("resize", resized);
$(window).ready(resized);

var tack = function tack(c,x,y,z,w,d,h) {
  x -= scrollX;
  y -= scrollY;

  var oz = -2*z;
  var sx = 2*(y-x), sy = y+x+oz;

  c.beginPath();
  c.moveTo(sx,sy);
  c.lineTo(sx-2*w,sy+w);
  c.moveTo(sx,sy);
  c.lineTo(sx+2*d,sy+d);
  c.moveTo(sx,sy);
  c.lineTo(sx,sy-2*h);
};

var cube = function cube(c,x1,y1,z,w,d,h) {
  x1 -= scrollX; // offset into the world
  y1 -= scrollY;

  var x2 = x1 + w;
  var y2 = y1 + d;

  var oz = -2*z;

  // these can be optimized
  var sx1 = 2*(y1-x1), sy1 = y1+x1+oz;
  var sx2 = 2*(y1-x2), sy2 = y1+x2+oz;
  var sx3 = 2*(y2-x2), sy3 = y2+x2+oz;
  var sx4 = 2*(y2-x1), sy4 = y2+x1+oz;
  var up = -2*h;

  c.beginPath();
  c.moveTo(sx3,sy3);
  c.lineTo(sx2,sy2);
  c.lineTo(sx2,sy2+up);
  c.lineTo(sx3,sy3+up);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
  c.lineTo(sx4,sy4+up);
  c.lineTo(sx1,sy1+up);
  c.lineTo(sx2,sy2+up);
  c.moveTo(sx3,sy3+up);
  c.lineTo(sx4,sy4+up);
};

var room = function cube(c,x1,y1,z,w,d,h) {
  x1 -= scrollX; // offset into the world
  y1 -= scrollY;

  var x2 = x1 + w;
  var y2 = y1 + d;

  var oz = -2*z;

  // these can be optimized
  var sx1 = 2*(y1-x1), sy1 = y1+x1+oz;
  var sx2 = 2*(y1-x2), sy2 = y1+x2+oz;
  var sx3 = 2*(y2-x2), sy3 = y2+x2+oz;
  var sx4 = 2*(y2-x1), sy4 = y2+x1+oz;
  var up = -2*h;

  c.beginPath();
  c.moveTo(sx1,sy1+up);
  c.lineTo(sx2,sy2+up);
  c.lineTo(sx2,sy2);
  c.lineTo(sx1,sy1);
  c.lineTo(sx1,sy1+up);
  c.lineTo(sx4,sy4+up);
  c.lineTo(sx4,sy4);
  c.lineTo(sx1,sy1);
  c.moveTo(sx2,sy2);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
};

var floor = function floor(c,x1,y1,z,w,d,h) {
  x1 -= scrollX; // offset into the world
  y1 -= scrollY;

  var x2 = x1 + w;
  var y2 = y1 + d;

  var oz = -2*z;

  // these can be optimized
  var sx1 = 2*(y1-x1), sy1 = y1+x1+oz;
  var sx2 = 2*(y1-x2), sy2 = y1+x2+oz;
  var sx3 = 2*(y2-x2), sy3 = y2+x2+oz;
  var sx4 = 2*(y2-x1), sy4 = y2+x1+oz;
  var up = -2*h;

  c.beginPath();
  c.moveTo(sx1,sy1);
  c.lineTo(sx2,sy2);
  c.lineTo(sx3,sy3);
  c.lineTo(sx4,sy4);
  c.lineTo(sx1,sy1);
};

var scratch = new ScreenPoint();
var cursor = new WorldPoint();

var s = shadows.canvas;
var c = foreground.canvas;

// show bounding boxes
var bounding = false;
var bounding_toggle = null;
var soft_shadows = false;

var player = new physics.Body( 0,0,0,0.5,0.5,2,100);
var image = new Image();
image.onload = function() {
  image.complete = true;
  console.log('image loaded');
};
image.src = 'images/sprites/books_what.png';
// image.src = 'file:///Users/ekmett/haskell/roguekcd/static/images/sprites/books_what.png';


player.elasticity = 0.8;
player.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
physics.bodies.push(player);
player.draw = function() {
  c.shadowBlur = 0;
  c.shadowColor = '#000';
  c.shadowOffsetX = 0;
  c.shadowOffsetY = 0;

  if (bounding) {
    tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
    c.lineWidth = 0.01;
    c.strokeStyle = "grey";
    c.stroke();
  }

  c.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);
  scratch.world(this.rx,this.ry,this.rz);
  c.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight,
    scratch.sx-1.25, scratch.sy-5.5, 2*Math.sqrt(3), 4*Math.sqrt(3)
  );

  s.setTransform(PIXELS_PER_METER,0,0,0.5*PIXELS_PER_METER,halfWidth,halfHeight);
  s.beginPath();
  scratch.world(this.rx,this.ry,0);
  s.arc(scratch.sx,scratch.sy*2+this.h,this.w*0.5*Math.sqrt(3),0,2*Math.PI,false);
  s.fillStyle = "rgba(0,0,0,0.25)";
  s.fill();
  s.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

  if (bounding) {
    cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
    c.lineWidth = 0.01;
    c.strokeStyle = "black";
    c.stroke();
  }
};
player.ai = function() {
  {
    var pdx = 0;
    var pdy = 0;
    var pdz = 0;

    var m = player.standing ? 1 : 0.2;

    if (events.impulse[87]) { pdx -= m; pdy -= m; } // W
    if (events.impulse[65]) { pdx += m; pdy -= m; } // A
    if (events.impulse[83]) { pdx += m; pdy += m; } // S
    if (events.impulse[68]) { pdx -= m; pdy += m; } // D
    if (player.jumpStart) {
      if (events.impulse[32] && player.standing) {
        // high jump
        pdz += 1.3;
      } else {
        // low jump
        pdz += 1;
      }
      player.jumpStart = false;
    } else if (events.impulse[32] && player.standing) {
      player.jumpStart = true;
    }
    bounding = events.impulse[66]; // B held
    soft_shadows = events.impulse[78]; // N held

    player.push(5*pdx,5*pdy,50*pdz);
  }

  if (events.impulse[69]) {
    // user clicked
    // update the cursor in world coordinates
    cursor.screen(
      (events.mouseX-halfWidth) * METERS_PER_PIXEL,
      (events.mouseY-halfHeight) * METERS_PER_PIXEL
    );

   var r = -Math.log(Math.random())/2.5+0.2;
   var body = new physics.Body(cursor.x-r/2, cursor.y-r/2, Math.random()*10,r,r,r,20*r^3);
   body.color = '#' + (0x1000000 + Math.random() * 0xFFFFFF).toString(16).substr(1,6);
   body.specular = 0.7; // the higher the number the larger the specular highlight.
   body.draw = function() {
     if (bounding) {
       tack(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
       c.lineWidth = 0.01;
       c.strokeStyle = this.color;
       c.stroke();
     }

     if (soft_shadows) {
       c.shadowColor = 'rgba(0,0,0,.8)';
       c.shadowBlur = 16*this.w;
       c.shadowOffsetX = 0; // 8*this.w;
       c.shadowOffsetY = 4*this.w;
     }

     c.beginPath();
     scratch.world(this.rx,this.ry,this.rz);
     c.arc(scratch.sx,scratch.sy,this.w*Math.sqrt(3),0,2*Math.PI,false);
     c.fillStyle = this.color;
     var g = c.createRadialGradient(scratch.sx,scratch.sy-this.h*Math.sqrt(2),0.1,scratch.sx,scratch.sy,this.w*Math.sqrt(3));
     g.addColorStop(0,"white");
     g.addColorStop(this.specular,this.color);
     c.fillStyle = g;
     c.lineWidth = 0.1;
     c.strokeStyle = "black";
     // experimental, slow, shadow
     c.stroke();
     c.fill();

     if (bounding) {
       cube(c,this.rx,this.ry,this.rz,this.w,this.d,this.h);
       c.lineWidth = 0.01;
       c.strokeStyle = "black";
       c.stroke();
     }

     s.setTransform(PIXELS_PER_METER,0,0,0.5*PIXELS_PER_METER,halfWidth,halfHeight);
     s.beginPath();
     scratch.world(this.rx,this.ry,0);
     s.arc(scratch.sx,scratch.sy*2+this.h*2,this.w*Math.sqrt(3),0,2*Math.PI,false);
     s.fillStyle = "rgba(0,0,0,0.25)";
     s.fill();
     s.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

   };
   var ty = Math.random();
   if (ty<0.25) {
     body.color = "#f33";
     body.lag = Math.random()*5-3;
     body.ai = function dog() {
       var dx = player.ox * body.lag + player.x * (1 - body.lag) - this.x;
       var dy = player.oy * body.lag + player.y * (1 - body.lag) - this.y;
       var dz = player.oz * body.lag + player.z * (1 - body.lag) - this.z;
       var l = Math.sqrt(dx*dx+dy*dy+dz*dz);
       if (Math.abs(l) > 0.01) {
         dx /= l;
         dy /= l;
         dz /= l;
         if (body.standing) this.push(dx,dy,this.w*20*dz+Math.random()*20); // this.w^3);
       }
     };
   } else if (ty<0.35) {
     body.color = "#ff0";
     body.lag = Math.random()*5-3;
     body.ai = function scared_dog() {
       var dx = player.ox * body.lag + player.x * (1 - body.lag) - this.x;
       var dy = player.oy * body.lag + player.y * (1 - body.lag) - this.y;
       var l = Math.sqrt(dx*dx+dy*dy)*3;
       if (Math.abs(l) > 0.01) {
         dx /= l;
         dy /= l;
         if (body.standing) this.push(-dx,-dy,0);
       }
     };
   } else if (ty < 0.45) {
     body.color = "#0ff";
     body.speed1 = Math.random()*4-1;
     body.speed2 = Math.random()*4-1;
     body.start = Math.random()*40;
     body.ai = function() {
       var dx = Math.sin((body.start+physics.frame)*3.14/20*body.speed1);
       var dy = Math.cos((body.start+physics.frame)*3.14/20*body.speed2);
       if (body.standing) this.push(dx*0.25,dy*0.25,0);
     }
   } else if (ty < 0.55) { // tar baby
     body.color = "#000";
     body.specular = 0.1;
     body.inverseMass /= 10;
     body.mass *= 10;
     body.elasticity = 0.01;
     body.constraints = 0; // TODO: track targets so we don't add them multiple times
     body.bump = function(that) {
       if (this.constraints < 4 && !that.constrained) {
         this.constraints++;
         that.constrained = true;
         var l = Math.min(this.w+that.w,this.d+that.d,this.h+that.h)/2;
         physics.constraints.push(constraints.stick(body,that,l*0.9));
       }
     }
   }
   physics.bodies.push(body);
  }
};

var genocide = $(".genocide-link").click(function() {
  physics.bodies = [player];
  physics.constraints = [];
});

var scratch = new ScreenPoint();

// window.setInterval( function() { return snapBy(1,1); }, 33);
var render = function render() {
  var t = performance.now();
  var pt = physics.updated;

  requestAnimationFrame(render);

  // no physics yet
  if (!pt) return;

  var alpha = (t - pt) / physics.MILLISECONDS_PER_FRAME;

  stats.display.begin();

  var b = background.canvas;
  b.clear(true);
  b.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);
  b.lineWidth = 0.3;

  var s = shadows.canvas;
  s.clear(true);
  s.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

  var c = foreground.canvas;
  c.clear(true);
  c.setTransform(PIXELS_PER_METER,0,0,PIXELS_PER_METER,halfWidth,halfHeight);

  // draw the world
  room(b,-5,-5,0,10,10,2);
  b.strokeStyle = "#ccc";
  b.fillStyle = "#ddd";
  b.fill();
  b.stroke();

  floor(b,-5,-5,0,10,10,2);
  b.fillStyle = "#fff";
  b.fill();

  /*
  tack(b,0,0,0,1,1,1);
  b.strokeStyle = "#0f0";
  b.stroke();
  */

  for (var i in physics.bodies) {
    var b = physics.bodies[i];
    b.interpolate(alpha);
  }

  physics.bodies.sort(function(a,b) {
    return a.key - b.key;
  });


  for (var i in physics.bodies) {
    var b = physics.bodies[i];
    b.draw();
  }

  stats.display.end();
}

render(performance.now());

return {
};

});
