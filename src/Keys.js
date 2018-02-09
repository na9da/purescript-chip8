/* global exports */
"use strict";

exports.onKeyUp = function (f) {
    return function () {
	addEventListener('keyup', function(e) { f(e.keyCode)(); })
    };
};

exports.onKeyDown = function (f) {
    return function () {
	addEventListener('keydown', function(e) { f(e.keyCode)(); })
    };
};


exports.getKeyImpl = function(f) {
    return function() {
	const cb = function(e) {
	    f(e.keyCode)();
	    removeEventListener('keydown', cb)
	}
	addEventListener('keydown', cb)
    }
}
