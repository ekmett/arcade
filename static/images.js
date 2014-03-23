define("images",
  ["jquery"],
  function ($) {

  var images = {
    cache: {}
  };

  var load = images.load = function(uri) {
    if (image.cache[uri]) {
      return image.cache[uri];
    }
    var img = new Image();
    img.onload = function() {
      image.cache[url] = img;
    };
    return img;
  };

  return images;
});
