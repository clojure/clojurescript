$ErrorActionPreference = "Stop"
$root = Resolve-Path $PSScriptRoot\..

$testjs = "builds/out-adv/core-advanced-test.js"

$targets =
    @{ env="V8_HOME"; name="V8"; cmd={ & "$env:V8_HOME\d8" $testjs } },
    @{ env="SPIDERMONKEY_HOME"; name="SpiderMonkey"; cmd={ & "$env:SPIDERMONKEY_HOME\js" -f $testjs } },
    @{ env="JSC_HOME"; name="JavaScriptCore"; cmd={ & "$env:JSC_HOME\jsc" -f $testjs } },
    @{ env="NASHORN_HOME"; name="Nashorn"; cmd={ & "$env:NASHORN_HOME\jjs" $testjs } },
    @{ env="CHAKRACORE_HOME"; name="ChakraCore"; cmd={ & "$env:CHAKRACORE_HOME\ch" $testjs } }
$ran = 0

$opts = $('{:optimizations :advanced :output-wrapper true :verbose true :compiler-stats true :parallel-build true :output-dir \"builds/out-adv\" :output-to \"' + $testjs + '\" :npm-deps {:lodash \"4.17.4\"} :closure-warnings {:non-standard-jsdoc :off :global-this :off} :install-deps true :language-in :es6 :language-out :es5 :foreign-libs [{:file \"src/test/cljs/calculator_global.js\" :provides [\"calculator\"] :global-exports {calculator Calculator}} {:file \"src/test/cljs/es6_dep.js\" :module-type :es6 :provides [\"es6_calc\"]} {:file \"src/test/cljs/calculator.js\" :module-type :commonjs :provides [\"calculator\"]} {:file \"src/test/cljs/es6_default_hello.js\" :provides [\"es6_default_hello\"] :module-type :es6}]}"')

function Test-It($env, $name, [scriptblock] $cmd) {
    $env_val = if(Test-Path env:$env) { (Get-Item env:$env).Value } else { "" }
    if("$env_val" -eq "") {
        Write-Host "$env not set, skipping $name tests"
    } else {
        Write-Host "Testing with $name"
        & $cmd
        $ran++
    }
}

Push-Location $root
try {
    "builds\out-adv", "out", "target" |
        Where-Object { Test-Path $_ -Type leaf } |
        Foreach-Object { Remove-Item $_ -recurse -force }

    New-Item builds\out-adv -ItemType Directory -Force | Out-Null

    bin\cljsc src\test\cljs $opts

    $targets | Foreach-Object { Test-It @_ }
}
finally {
    Pop-Location

    Write-Host "Tested with $ran out of $($targets.Length) possible js targets"
}