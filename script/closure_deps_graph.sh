#!/usr/bin/env bash

set -e

if [[ -z "$CLJS_SCRIPT_QUIET" ]]; then
  set -x
fi

FILE_SEP='/'
PATH_SEP=':'
OS_ID=`uname | tr [:upper:] [:lower:]`
CLJS_SCRIPT_MVN_OPTS=${CLJS_SCRIPT_MVN_OPTS:-""}

if [[ $OS_ID == *mingw* ]]
then
    echo "MINGW detected"
    # Refer to http://www.mingw.org/wiki/Posix_path_conversion
    FILE_SEP='//'
    PATH_SEP=';'
fi

CP_FILE=`mktemp /tmp/cljs_cp.txt.XXXXXXXXXXX`

mvn -ntp -B -f ../../pom.template.xml dependency:build-classpath -Dmdep.outputFile=$CP_FILE -Dmdep.fileSeparator=$FILE_SEP -Dmdep.pathSeparator=$PATH_SEP $CLJS_SCRIPT_MVN_OPTS

CLJS_CP=`cat $CP_FILE`

# For Hudson server
if [ "$HUDSON" = "true" ]; then
    $JAVA_HOME/bin/java -server -cp "$CLJS_CP""$PATH_SEP""src/main/clojure" clojure.main ../closure_deps_graph.clj
else
    java -server -cp "$CLJS_CP""$PATH_SEP""src/main/clojure" clojure.main ../closure_deps_graph.clj
fi
