#!/usr/bin/env bash

set -e

## Set the version numbers to download and release:

ZIP_VERSION="20111110-r1376"
RELEASE_VERSION="0.0-1376-2"

## These only need to change if the URL or file names change:

ZIP_BASE="closure-library-${ZIP_VERSION}"
ZIP_FILE="${ZIP_BASE}.zip"
ZIP_URL="http://closure-library.googlecode.com/files/${ZIP_FILE}"

RELEASE_BASE="google-closure-library-${RELEASE_VERSION}"
JAR_FILE="$RELEASE_BASE.jar"
POM_FILE="$RELEASE_BASE.pom"

THIRD_PARTY_RELEASE_BASE="google-closure-library-third-party-${RELEASE_VERSION}"
THIRD_PARTY_JAR_FILE="$THIRD_PARTY_RELEASE_BASE.jar"
THIRD_PARTY_POM_FILE="$THIRD_PARTY_RELEASE_BASE.pom"

POM_TEMPLATE_FILE="google-closure-library.pom.template"
THIRD_PARTY_POM_TEMPLATE_FILE="google-closure-library-third-party.pom.template"

## Main script begins:

cd `dirname $0`

DATE=`date "+%Y%m%d%H%M%S"`
WORKING="closure-release-${DATE}"

rm -rf "$WORKING"
mkdir "$WORKING"

if [ ! -e "$ZIP_FILE" ]; then
    curl "$ZIP_URL" -o "$ZIP_FILE"
fi

if [ ! -d "$WORKING/$ZIP_BASE" ]; then
    ( cd "$WORKING" && unzip "../$ZIP_FILE" )
fi

cd "$WORKING"

## Modify deps.js for third-party JAR; see CLJS-276:

perl -p -i -e 's/..\/..\/third_party\/closure\/goog\///go' \
    closure/goog/deps.js

rm -f ./third_party/closure/goog/base.js \
    ./third_party/closure/goog/deps.js

## Build the JARs:

jar cf "$JAR_FILE" \
    AUTHORS \
    LICENSE \
    README \
    -C closure goog \
    -C closure css

jar cf "$THIRD_PARTY_JAR_FILE" \
    AUTHORS \
    LICENSE \
    README \
    -C third_party/closure goog

## Generate the POM files:

perl -p -e "s/RELEASE_VERSION/$RELEASE_VERSION/go" \
    "../$POM_TEMPLATE_FILE" \
    > "$POM_FILE"

perl -p -e "s/RELEASE_VERSION/$RELEASE_VERSION/go" \
    "../$THIRD_PARTY_POM_TEMPLATE_FILE" \
    > "$THIRD_PARTY_POM_FILE"

## Uncomment these lines for an official release:

# for FILE in "$JAR_FILE" "$THIRD_PARTY_JAR_FILE" "$POM_FILE" "$THIRD_PARTY_POM_FILE"
# do
#     gpg --verbose --armor --detach-sign \
#         --default-key "Clojure/core (build.clojure.org Release Key version 2) <core@clojure.com>" \
#         "$FILE"
# done
