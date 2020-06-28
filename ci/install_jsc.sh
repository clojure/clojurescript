#!/usr/bin/env bash
if [ ! -d WebKit ]
then
  git clone --depth=1 git://git.webkit.org/WebKit.git WebKit;
  cd WebKit;
  Tools/Scripts/build-jsc --jsc-only;
  cd ..
fi
