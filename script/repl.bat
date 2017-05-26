@echo off
setLocal EnableDelayedExpansion

if "%CLOJURESCRIPT_HOME%" == "" set CLOJURESCRIPT_HOME=%~dp0..\

set CLASSPATH=%CLOJURESCRIPT_HOME%\src\clj;%CLOJURESCRIPT_HOME%\src\cljs"
for /R "%CLOJURESCRIPT_HOME%\lib" %%a in (*.jar) do (
  set CLASSPATH=!CLASSPATH!;%%a
)
set CLASSPATH=!CLASSPATH!"

java -server -cp "%CLASSPATH%" clojure.main

