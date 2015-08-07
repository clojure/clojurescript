Math.imul = function(a, b) {};

var cljs = {};
cljs.core = {};
/**
 * @constructor;
 */
cljs.core.Iterator = function() {};
cljs.core.Iterator.prototype.next = function() {};

/**
 * @constructor;
 */
function IteratorStep() {};
/**
 * @type {boolean}
 */
IteratorStep.prototype.done;
/**
 * @type {Object}
 */
IteratorStep.prototype.value;

/**
 * @constructor;
 */
function IEquiv() {};
IEquiv.prototype.equiv = function() {};
