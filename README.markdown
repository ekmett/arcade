arcade
------

[![Build Status](https://secure.travis-ci.org/ekmett/arcade.png?branch=master)](http://travis-ci.org/ekmett/arcade)

This is a web-based game physics playground.

Currently this is just a placeholder while I work out how much game content I can present through an HTML5 Canvas.

Controls:

* `1`: Spawns an inert "water balloon" of random size.
* `2`: Spawns an "angry" red ball of random size.
* `3`: Spawns a yellow ball of random size that is afraid of you.
* `4`: Spawns a cyan balloon.
* `5`: Spawns a "tar baby", which grabs other objects that touch it.
* `6`: Spawns a plastic zombie that wants to eat your brains.
* `7`: Spawns a spider. Spiders can 'socket'
* `8`: Spawns a cable of random length.
* `0`: Genocide: Clear all spawns.
* `W`,`A`,`S`,`D` - movement
* You can grab things with the mouse and move them.
* `Q` raises whatever you have grabbed, `E` lowers it.
* Holding `C` turns on ambient occlusion.
* Holding `B` turns on bounding boxes.

Physics is verlet integrated, enabling us to easily play with constraints between particles.

Particles are treated as bounding boxes that upon collisions push against each other using a fast approximation
of ellipsoid collision based on sphere collision in a distorted space.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
