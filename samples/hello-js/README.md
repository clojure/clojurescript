*NOTE: this sample is now out of date. Please refer to the Quick Start*

Simple ClojureScript Project Example Using an External JavaScript Library

One-time Setup
==============

- Create a CLOJURESCRIPT_HOME environment variable which points to the
  ClojureScript root directory.

- If you have not already done so, execute 

    $CLOJURESCRIPT_HOME/script/bootstrap

- Add $CLOJURESCRIPT_HOME/bin to your PATH.


Simple external JavaScript file
===============================



Run in Development Mode
-----------------------

Development mode allows for each file to be loaded in a separate script tag so
that errors can be easily tracked to the offending file.

    cljsc src > hello-js.js

After running the above command, open hello-js-dev.html. Notice that each required
JavaScript file has been loaded in its most readable form. 

Run Optimized JavaScript
------------------------

Once an application is ready for production, a single optimized file can be produced.

    cljsc src {:optimizations :advanced} > hello-js.js

After running the above command, open hello-js.html to view the result.


Using an externed JavaScript file
=================================

To see how external calls are optimized away, execute the following:

    cljsc src '{:optimizations :advanced :output-to "hello-extern.js"}'

After running the above command, open hello-extern.html to view the result.  You should see nothing and possibly a JavaScript error.  The solution is to compile the ClojureScript source with a referred externs file as follows:

    cljsc src '{:optimizations :advanced :output-to "hello-extern.js" :externs ["externs.js"]}'

Again after running the above command, open hello-extern.html to view the result.  You should see an alert box.
