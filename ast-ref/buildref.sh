#!/bin/sh

java -cp .:`lein cp` clojure.main <<EOF
 (load "gen-ref")
 (System/exit 0)
EOF

#git pull
#mv quickref.html q.html
#git checkout origin/gh-pages
#mv q.html quickref.html
#git add quickref.html
#git commit -m "update quickref"
#git push origin HEAD:gh-pages
#git checkout master
