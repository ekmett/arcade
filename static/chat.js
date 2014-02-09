define(['jquery'],function($) {

var div = $("footer");

// conditionally scroll on scroll wheel
function scroll() {
  div.scrollTop(div.innerHeight());
  console.log("scrolling chat");
};

$(window).ready(scroll);
$(document).bind("resize",scroll);

return {

};

});
