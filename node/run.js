#!/usr/bin/env node

// Launch a CLJS compiled program using node
// - First argument is the path to the compiled *.js file
// - Rest of the arguments are passed to the *main-cli*

(function() {
    var path = require("path"),
        targ = process.argv[2],
        ns = path.basename(targ).replace(/[.]js$/, ""),
        jsfile = path.resolve("./", targ);

    // Load the Closure Library wrapper
    require('./out/goog.js');
    
    // Load the initial compiled file
    require(jsfile);
    goog.require(ns);

    // Setup the print function
    cljs.core._STAR_print_fn_STAR_ = require("util").print;

    // Bootstrap cljs.user namespace
    cljs.core.swap_BANG_.call(null,cljs.compiler._STAR_emitted_provides_STAR_,cljs.core.conj,(new cljs.core.Symbol(null,"cljs.user")));
    goog.provide('cljs.user');
})();

// Call the users's main function
cljs.core.apply.call(null,cljs.core._STAR_main_cli_fn_STAR_,cljs.core.drop.call(null,3,process.argv));

