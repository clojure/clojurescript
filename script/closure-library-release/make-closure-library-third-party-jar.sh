#!/usr/bin/env bash

set -e 

BASENAME=google-closure-library-third-party-0.0-1376

jar cf $BASENAME.jar \
    -C ../closure-library-20111110-r1376 AUTHORS \
    -C ../closure-library-20111110-r1376 LICENSE \
    -C ../closure-library-20111110-r1376 README \
    -C ../closure-library-20111110-r1376/third_party/closure goog


## Uncomment these lines for an official release:

# gpg --verbose --armor --detach-sign \
#     --default-key "Clojure/core (build.clojure.org Release Key version 2) <core@clojure.com>" \
#     $BASENAME.jar

# gpg --verbose --armor --detach-sign \
#     --default-key "Clojure/core (build.clojure.org Release Key version 2) <core@clojure.com>" \
#     $BASENAME.pom

