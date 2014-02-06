define([], function() {
  var Buffer = function(n,content,grow) {
    this.available = n
    this.length = 0;
    this.content = content;
    this.grow = grow;
  };

  Buffer.prototype = {
    alloc : function(k) {
      var n = this.length;
      var r = n + (typeof k === 'undefined' ? 1 : k);
      while (r > this.available) {
        this.available *= 2;
        this.content = this.grow(this.content, n);
      }
      this.length = r;
      return n;
    }
  };

  return {
    Buffer: Buffer;
  }
});
