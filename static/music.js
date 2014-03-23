define('music',
  ['jquery'],
  function($) {

  var music = $("#music");

  return {
    play: function() {
      music[0].play();
    },
    stop: function() {
      music[0].stop();
    }
  }
});
