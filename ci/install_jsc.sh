#!/usr/bin/env bash
if [ ! -d WebKit ]
then
  git clone -b Safari-611.1.5.1 --depth=1 https://github.com/WebKit/WebKit.git WebKit;
  cd WebKit;
  Tools/Scripts/build-jsc --jsc-only;
  cd ..
fi
