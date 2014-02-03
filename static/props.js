define("props",
  [],
  function() {

  var props = {};

  var Prop = props.Prop = function(opaque, translucent, collisions, translucencies) {
    this.opaque         = opaque; // opaque img
    this.translucent    = translucent; // translucent img for when something 'important' overlaps the translucency region.
    this.collisions     = collisions || []; // y-monotone collision polygons
    this.translucencies = transluencies || []; // y-monotone transparency polygons
  };
});
