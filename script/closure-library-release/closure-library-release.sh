#!/usr/bin/env bash

# closure-library-release.sh

# ClojureScript depends on the Google Closure JavaScript Libraries,
# but Google does not publish those libraries in a Maven repository.
# This script builds generates Maven projects for the Google Closure
# Library and, optionally, deploys them.

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


set -e

### Constants

POM_TEMPLATE_FILE="google-closure-library.pom.template"
THIRD_PARTY_POM_TEMPLATE_FILE="google-closure-library-third-party.pom.template"
GIT_CLONE_URL="git@github.com:google/closure-library.git"

### Functions

function print_usage {
    echo "Usage: ./make-closure-library-jars.sh <directory>

<directory> is the root directory of the Google Closure library
Git repository."
}

### MAIN SCRIPT BEGINS HERE

## Command-line validation

if [[ ! -e $POM_TEMPLATE_FILE || ! -e $THIRD_PARTY_POM_TEMPLATE_FILE ]]; then
    echo "This script must be run from the directory containing
google-closure-library.pom.template and
google-closure-library-third-party.pom.template"
    exit 1
fi

## Fetch the Git repo

closure_library_dir="closure-library"

if [[ ! -d $closure_library_dir ]]; then
    git clone "$GIT_CLONE_URL" "$closure_library_dir"
fi

(
    cd "$closure_library_dir"
    git clean -fdx
    git checkout master
    git pull
)

closure_library_base="$closure_library_dir/closure"
third_party_base="$closure_library_dir/third_party/closure"

if [[ ! -d $closure_library_base || ! -d $third_party_base ]]; then
    echo "$closure_library_dir does not look like the Closure library"
    exit 1
fi

## Working directory

now=$(date "+%Y%m%d%H%M%S")
work_dir="tmp-build"

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

## Creating directories

project_dir="$work_dir/google-closure-library"
src_dir="$project_dir/src/main/resources"

third_party_project_dir="$work_dir/google-closure-library-third-party"
third_party_src_dir="$third_party_project_dir/src/main/resources"

mkdir -p "$src_dir" "$third_party_src_dir"

## Copy Closure sources

cp -r \
    "$closure_library_dir/AUTHORS" \
    "$closure_library_dir/LICENSE" \
    "$closure_library_dir/README.md" \
    "$closure_library_dir/closure/goog" \
    "$closure_library_dir/closure/css" \
    "$src_dir"

cp -r \
    "$closure_library_dir/AUTHORS" \
    "$closure_library_dir/LICENSE" \
    "$closure_library_dir/README.md" \
    "$closure_library_dir/third_party/closure/goog" \
    "$third_party_src_dir"

## Modify main deps.js for third-party JAR; see CLJS-276:

perl -p -i -e 's/..\/..\/third_party\/closure\/goog\///go' \
    "$src_dir/goog/deps.js"

## Remove empty third-party deps.js and base.js

rm -f \
    "$third_party_src_dir/goog/deps.js" \
    "$third_party_src_dir/goog/base.js"

## Generate the POM files:

perl -p -e "s/RELEASE_VERSION/$release_version/go" \
    "$POM_TEMPLATE_FILE" \
    > "$project_dir/pom.xml"

perl -p -e "s/RELEASE_VERSION/$release_version/go" \
    "$THIRD_PARTY_POM_TEMPLATE_FILE" \
    > "$third_party_project_dir/pom.xml"

## Deploy the files if we are on Hudson

if [ "$HUDSON" = "true" ]; then
    (
        cd "$third_party_project_dir"
        mvn --fail-at-end \
            -Psonatype-oss-release \
            -Dsource.skip=true \
            clean deploy
    )

    (
        cd "$project_dir"
        mvn --fail-at-end \
            -Psonatype-oss-release \
            -Dsource.skip=true \
            clean deploy
    )

    echo "Now log in to https://oss.sonatype.org/ to release"
    echo "the staging repository."
else
    echo "Not on Hudson, local Maven install"
    (
        cd "$third_party_project_dir"
        mvn clean
        mvn package
        mvn install:install-file -Dfile=./target/google-closure-library-third-party-$release_version.jar -DpomFile=pom.xml
    )
    (
        cd "$project_dir"
        mvn clean
        mvn package
        mvn install:install-file -Dfile=./target/google-closure-library-$release_version.jar -DpomFile=pom.xml
    )
fi
