#!/usr/bin/env node

// Launch a CLJS compiled program using node
// - First argument is the path to the compiled *.js file
// - Rest of the arguments are passed to the *main-cli*

(function() {
    var path = require("path"),
        targ = process.argv[2],
        ns = path.basename(targ).replace(/[.]js$/, ""),
        jsfile = path.resolve("./", targ);

    require('./out/goog.js');
    require(jsfile);
    goog.require(ns);

    cljs.core._STAR_print_fn_STAR_ = require("util").print;
    bs.reset();
    cljs.user = {};
    env = cljs.analyzer.empty_env();
})();

// Call the users's main function
cljs.core.apply.call(null,cljs.core._STAR_main_cli_fn_STAR_,cljs.core.drop.call(null,3,process.argv));

