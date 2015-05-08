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
function Map() {};
Map.prototype.keys = function() {};
Map.prototype.entries = function() {};
Map.prototype.values = function() {};
Map.prototype.has = function(k) {};
Map.prototype.get = function(k) {};
Map.prototype.forEach = function(f) {};

/**
 * @constructor;
 */
function Set() {};
Set.prototype.keys = function() {};
Set.prototype.entries = function() {};
Set.prototype.values = function() {};
Set.prototype.has = function(k) {};
Set.prototype.forEach = function(f) {};

/**
 * @constructor;
 */
function IEquiv() {};
IEquiv.prototype.equiv = function() {};
