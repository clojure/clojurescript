#!/usr/bin/env bash

# make-closure-library-jars.sh

# ClojureScript depends on the Google Closure JavaScript Libraries,
# but Google does not publish those libraries in a Maven repository.
# This script builds release JAR and POM files for the Google Closure
# Library and its third-party extensions.

# The Google Closure Libraries are divided into two parts: the main
# library and third-party extensions. The main library is Apache
# licensed; the third-party extensions have a variety of different
# licenses. However, code in the main library depends on the
# third-party extensions, not the other way around. See CLJS-418 for
# details.

# To manage this, we build two JARs, google-closure-library and
# google-closure-library-third-party, with the former declaring an
# explicit dependency on the latter. This permits consumers to exclude
# the third-party libraries (and their various licenses) if they know
# they don't need them.

# To match this structure, we need to alter the deps.js file that the
# Google Closure Compiler uses to resolve dependencies. See CLJS-276
# for details.

# The last release ZIP made by Google was 20130212-95c19e7f0f5f. To
# get newer versions, we have to go to the Git repository.

# Usage:

# 1. Clone the Google Closure Library Git repository
#
# 2. cd to the directory containing this script
#
# 3. Run this script with the path to the G.Closure Library
#    as a command-line argument

# If you are a Clojure release admin (you have the GPG key) then set
# the environment variable SIGN_GOOGLE_CLOSURE_LIBRARY_RELEASE to sign
# the releases with GPG.



set -e

### Constants

POM_TEMPLATE_FILE="google-closure-library.pom.template"
THIRD_PARTY_POM_TEMPLATE_FILE="google-closure-library-third-party.pom.template"
RELEASE_KEY="Clojure/core (build.clojure.org Release Key version 2) <core@clojure.com>"

### Functions

function print_usage {
    echo "Usage: ./make-closure-library-jars.sh <directory>

<directory> is the root directory of the Google Closure library
Git repository."
}

### MAIN SCRIPT BEGINS HERE

## Command-line validation

closure_library_dir="$1"

if [[ ! -e $POM_TEMPLATE_FILE || ! -e $THIRD_PARTY_POM_TEMPLATE_FILE ]]; then
    echo "This script must be run from the directory containing
google-closure-library.pom.template and
google-closure-library-third-party.pom.template"
    exit 1
fi

if [[ ! -d $closure_library_dir ]]; then
    print_usage
    exit 1
fi

closure_library_base="$closure_library_dir/closure"
third_party_base="$closure_library_dir/third_party/closure"

if [[ ! -d $closure_library_base || ! -d $third_party_base ]]; then
    echo "$closure_library_dir does not look like the Closure library"
    print_usage
    exit 1
fi

## Working directory

now=$(date "+%Y%m%d%H%M%S")
work_dir="closure-release-${now}"

echo "Working directory: $work_dir" 

rm -rf "$work_dir"
mkdir "$work_dir"

## Git parsing for release version

commit_details=$(cd "$closure_library_dir"; git log -n 1 '--pretty=format:%H %ci')

commit_short_sha=${commit_details:0:8}
commit_date="${commit_details:41:4}${commit_details:46:2}${commit_details:49:2}"
release_version="0.0-${commit_date}-${commit_short_sha}"

echo "HEAD commit: $commit_details"
echo "Date: $commit_date"
echo "Short SHA: $commit_short_sha"
echo "Release version: $release_version"

release_base="google-closure-library-${release_version}"
jar_file="${release_base}.jar"
pom_file="${release_base}.pom"

third_party_release_base="google-closure-library-third-party-${release_version}"
third_party_jar_file="${third_party_release_base}.jar"
third_party_pom_file="${third_party_release_base}.pom"

## Copy Closure source into working dir

mkdir "$work_dir/closure"
cp -r \
    "$closure_library_dir/AUTHORS" \
    "$closure_library_dir/LICENSE" \
    "$closure_library_dir/README" \
    "$closure_library_dir/closure/goog" \
    "$closure_library_dir/closure/css" \
    "$work_dir/closure"

mkdir "$work_dir/third_party"
cp -r \
    "$closure_library_dir/AUTHORS" \
    "$closure_library_dir/LICENSE" \
    "$closure_library_dir/README" \
    "$closure_library_dir/third_party/closure/goog" \
    "$work_dir/third_party"


## Modify deps.js for third-party JAR; see CLJS-276:

perl -p -i -e 's/..\/..\/third_party\/closure\/goog\///go' \
    "$work_dir/closure/goog/deps.js"

rm -f \
    "$work_dir/third_party/goog/base.js" \
    "$work_dir/third_party/goog/deps.js" \
    "$work_dir/third_party/closure/goog/base.js" \
    "$work_dir/third_party/closure/goog/deps.js"

## Build the JARs:

(
    cd "$work_dir/closure"
    jar cf "../$jar_file" *
)

(
    cd "$work_dir/third_party"
    jar cf "../$third_party_jar_file" *
)

## Generate the POM files:

perl -p -e "s/RELEASE_VERSION/$release_version/go" \
    "$POM_TEMPLATE_FILE" \
    > "$work_dir/$pom_file"

perl -p -e "s/RELEASE_VERSION/$release_version/go" \
    "$THIRD_PARTY_POM_TEMPLATE_FILE" \
    > "$work_dir/$third_party_pom_file"

## Sign the files with GPG

if [[ -n "$SIGN_GOOGLE_CLOSURE_LIBRARY_RELEASE" ]]; then
    (
        cd "$work_dir"
        for file in \
            "$jar_file" \
            "$third_party_jar_file" \
            "$pom_file" \
            "$third_party_pom_file"
        do
            gpg --verbose --armor --detach-sign \
                --default-key "$RELEASE_KEY" \
                "$file"
        done
    )
fi
