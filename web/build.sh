#!/bin/bash

cd $(dirname ${0})

echo "Building jsrepl.js"
time ../bin/cljsc ../src/cljs/jsrepl.cljs > jsrepl.js
