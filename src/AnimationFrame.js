/* global exports */
"use strict";

exports.requestAnimationFrame = function(action) {
   return function() {
     window.requestAnimationFrame(action)
   }
}



