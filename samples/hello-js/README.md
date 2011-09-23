Simple ClojureScript Project Example Using an External JavaScript Library

One-time Setup
==============

- Create a CLOJURESCRIPT_HOME environment variable which points to the
  ClojureScript root directory.

- If you have not already done so, execute 

    $CLOJURESCRIPT_HOME/script/bootstrap

- Add $CLOJURESCRIPT_HOME/bin to your PATH.

Run in Development Mode
=======================

Development mode allows for each file to be loaded in a separate script tag so
that errors can be easily tracked to the offending file.

        cljsc src > hello-js.js

After running the above command, open hello-js-dev.html. Notice that each required
JavaScript file has been loaded in its most readable form. 

Run Optimized JavaScript
========================

Once an application is ready for production, a single optimized file can be produced.

    cljsc src {:optimizations :advanced} > hello-js.js

After running the above command, open hello-js.html to view the result.
