
@echo off
setLocal EnableDelayedExpansion

if "%CLOJURESCRIPT_HOME%" == "" set CLOJURESCRIPT_HOME=%~dp0..\

set CLASSPATH=%CLOJURESCRIPT_HOME%src\clj;%CLOJURESCRIPT_HOME%src\cljs"
for /R "%CLOJURESCRIPT_HOME%\lib" %%a in (*.jar) do (
  set CLASSPATH=!CLASSPATH!;%%a
)
set CLASSPATH=!CLASSPATH!"

if (%1) == () (
echo Usage: "cljsc <file-or-dir> > out.js"
echo        "cljsc <file-or-dir> {:optimiztions :advanced} > out.js"
) else (
java -server -cp "%CLASSPATH%" clojure.main "%CLOJURESCRIPT_HOME%\bin\cljsc.clj" %*
)
