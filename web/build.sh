#!/bin/bash

cd $(dirname ${0})

echo "Building webrepl.js"
time ../bin/cljsc ../src/cljs/webrepl.cljs > webrepl.js
