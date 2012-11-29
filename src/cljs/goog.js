// Simple module wrapper around Google Closure Library
// goog/ directory should be in the same directory as this file

goog = exports;
path_ = require("path");
googDir_ = path_.join(path_.dirname(module.filename), "goog");

rawLoaded_ = {};
rawLoad_ = function(file) {
  var path = path_.resolve(googDir_, file);
  //console.log("rawLoad_ file:", file, "path:", path);

  if (rawLoaded_[path]) { return; }
  rawLoaded_[path] = true;

  var contents = require('fs').readFileSync(path);
  // TODO: cljs.nodejscli needs require, but this is gross
  global.require = require;
  process.binding('evals').NodeScript.
      runInThisContext.call(global, contents, file);
};

rawLoad_('base.js');
goog.global = goog.window = global.top = global;
rawLoad_('deps.js');

// Override goog.require script loader/evaluator
goog.writeScriptTag_ = function(file) {
  try {
    rawLoad_(file);
  } catch (exc) {
    console.error('Could not goog.require("' + file + '")\n' + exc.stack);
    process.exit(1);
  }
  return false;
};

