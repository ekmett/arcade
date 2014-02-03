define("image",
  ["jquery"],
  function ($) {

  var image = {
    cache: {}
  };

  var load = image.load = function(uri) {
    if (image.cache[uri]) {
      return image.cache[uri];
    }
    var img = new Image();
    img.onload = function() {
      image.cache[url] = img;
    };
    return img;
  };

  return image;
});
