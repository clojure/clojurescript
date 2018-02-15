var global = this; // required by React

var nashorn_load = function(path) {
    var outputPath = (typeof CLJS_OUTPUT_DIR != "undefined" ? CLJS_OUTPUT_DIR : ".") + java.io.File.separator + path;
    if (typeof CLJS_DEBUG != "undefined" && CLJS_DEBUG) print("loading:" + outputPath);
    load(outputPath);
};

goog.global.CLOSURE_IMPORT_SCRIPT = function(path) {
    nashorn_load("goog/" + path);
    return true;
};

goog.global.isProvided_ = function(name) { return false; };

// https://blogs.oracle.com/nashorn/setinterval-and-settimeout-javascript-functions

var __Platform = Java.type("javafx.application.Platform");
var __PImpl    = Java.type("com.sun.javafx.application.PlatformImpl");
var __Timer    = Java.type("java.util.Timer");

__PImpl.startup(function(){}); // init JavaFX

var nashorn_tear_down = function() {
    __Platform.exit();
}

function setTimerRequest(handler, delay, interval, args) {
    handler = handler || function() {};
    delay = delay || 0;
    interval = interval || 0;
    var applyHandler = function() { handler.apply(this, args); }
    var runLater = function() { __Platform.runLater(applyHandler); }
    var timer = new __Timer("setTimerRequest", true);
    if (interval > 0) {
        timer.schedule(runLater, delay, interval);
    } else {
        timer.schedule(runLater, delay);
    }
    return timer;
}

function clearTimerRequest(timer) {
    timer.cancel();
}

function setInterval() {
    var args = Array.prototype.slice.call(arguments);
    var handler = args.shift();
    var ms = args.shift();
    return setTimerRequest(handler, ms, ms, args);
}

function clearInterval(timer) {
    clearTimerRequest(timer);
}

function setTimeout() {
    var args = Array.prototype.slice.call(arguments);
    var handler = args.shift();
    var ms = args.shift();

    return setTimerRequest(handler, ms, 0, args);
}

function clearTimeout(timer) {
    clearTimerRequest(timer);
}

function setImmediate() {
    var args = Array.prototype.slice.call(arguments);
    var handler = args.shift();

    return setTimerRequest(handler, 0, 0, args);
}

function clearImmediate(timer) {
    clearTimerRequest(timer);
}