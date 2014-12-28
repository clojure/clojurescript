#!/bin/bash

rm -rf classes
mkdir classes
repl -e "(compile 'cljs.repl) (compile 'cljs.core)"