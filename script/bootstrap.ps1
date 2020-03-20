$ErrorActionPreference = "Stop"
$root = Resolve-Path "$PSScriptRoot\.."
$shell = New-Object -com shell.application

# Read white listed dependency version info from the /bin/sh script and store in variables
Get-Content $root\script\bootstrap |
    Where-Object { $_ -match '^\s*(\w+)\s*=\s*\"([^\"]*)\"\s*$' } |
    Where-Object { $matches[1] -in "CLOJURE_RELEASE", "SPEC_ALPHA_RELEASE",
        "CORE_SPECS_ALPHA_RELEASE", "CLOSURE_RELEASE", "DJSON_RELEASE",
        "TRANSIT_RELEASE", "GCLOSURE_LIB_RELEASE", "RHINO_RELEASE",
        "TREADER_RELEASE", "TEST_CHECK_RELEASE" } |
    Foreach-Object { New-Variable $matches[1] $matches[2] -Scope private }

function Get-WebResource($url, $dstPath) {
    Write-Verbose "Downloading '$url' -> '$dstPath'"
    Invoke-RestMethod $url -OutFile $dstPath
}

function Expand-ZipFile($srcPath, $dstDir, $items) {
    Write-Verbose "Unzipping '$srcPath'"

    function Get-ShellFolder($dir) {
        $folder = $shell.NameSpace($dir)
        if($folder -eq $null) {
            throw "Failed to bind to folder '$dir'"
        }
        $folder
    }

    function Copy-ShellItem([Parameter(ValueFromPipeline=$true)] $src) {
        process {
            Write-Verbose "Expanding '$($src.Path)' -> '$dstDir'"
            $dstFolder.CopyHere($src, 4 + 16 + 1024)
        }
    }

    function Parse-ShellItem([Parameter(ValueFromPipeline=$true)] $name) {
        process {
            $x = $srcFolder.ParseName($name)
            if($x -eq $null) {
                throw "Failed fo find item '$name' in zip file '$srcPath'"
            }
            $x
        }
    }

    $srcFolder = Get-ShellFolder($srcPath)
    $dstFolder = Get-ShellFolder($dstDir)

    if($items -ne $null) {
        $items | Parse-ShellItem | Copy-ShellItem
    }
    else {
        $srcFolder.Items() | Copy-ShellItem
    }
}

function Move-File($srcPath, $dstPath) {
    Delete-File $dstPath
    Write-Verbose "Moving '$srcPath' -> '$dstPath'"
    Move-Item $srcPath $dstPath
}

function Copy-File($srcPath, $dstPath) {
    Delete-File $dstPath
    Write-Verbose "Copying '$srcPath' -> '$dstPath'"
    Copy-Item $srcPath $dstPath
}

function Delete-File([Parameter(ValueFromPipeline=$true)] $path)
{
    process {
        if(Test-Path $path) {
            Write-Verbose "Deleting '$path'"
            Remove-Item $path -Recurse
        }
    }
}

function Make-Dir($dir) {
    if(!(Test-Path $dir -Type Container)) {
        Write-Verbose "Making directory '$dir'"
        New-Item $dir -ItemType Directory | Out-Null
    }
}

Make-Dir $root\lib
Make-Dir $root\closure\library
Make-Dir $root\closure\compiler

Write-Host "Fetching Clojure..."
Get-WebResource  `
    https://repo1.maven.org/maven2/org/clojure/clojure/$CLOJURE_RELEASE/clojure-$CLOJURE_RELEASE.jar `
    $root\lib\clojure-$CLOJURE_RELEASE.jar

Write-Host "Fetching specs.alpha...."
Get-WebResource  `
    https://repo1.maven.org/maven2/org/clojure/spec.alpha/$SPEC_ALPHA_RELEASE/spec.alpha-$SPEC_ALPHA_RELEASE.jar `
    $root\lib\spec.alpha-$SPEC_ALPHA_RELEASE.jar

Write-Host "Fetching core.specs.alpha...."
Get-WebResource  `
    https://repo1.maven.org/maven2/org/clojure/core.specs.alpha/$CORE_SPECS_ALPHA_RELEASE/core.specs.alpha-$CORE_SPECS_ALPHA_RELEASE.jar `
    $root\lib\core.specs.alpha-$CORE_SPECS_ALPHA_RELEASE.jar

Write-Host "Fetching data.json..."
Get-WebResource `
    https://repo1.maven.org/maven2/org/clojure/data.json/$DJSON_RELEASE/data.json-$DJSON_RELEASE.jar `
    $root\lib\data.json-$DJSON_RELEASE.jar

Write-Host "Fetching transit-clj..."
Get-WebResource `
    https://repo1.maven.org/maven2/com/cognitect/transit-clj/$TRANSIT_RELEASE/transit-clj-$TRANSIT_RELEASE.jar `
    $root\lib\transit-clj-$TRANSIT_RELEASE.jar

# TODO: Implement Closure SVN support
Write-Host "Fetching Google Closure library..."
Get-WebResource `
    https://repo1.maven.org/maven2/org/clojure/google-closure-library/$GCLOSURE_LIB_RELEASE/google-closure-library-$GCLOSURE_LIB_RELEASE.jar `
    $root\lib\google-closure-library-$GCLOSURE_LIB_RELEASE.jar
Get-WebResource `
    https://repo1.maven.org/maven2/org/clojure/google-closure-library-third-party/$GCLOSURE_LIB_RELEASE/google-closure-library-third-party-$GCLOSURE_LIB_RELEASE.jar `
    $root\lib\google-closure-library-third-party-$GCLOSURE_LIB_RELEASE.jar

Write-Host "Fetching Google Closure compiler..."
Get-WebResource `
    https://repo1.maven.org/maven2/com/google/javascript/closure-compiler/v$CLOSURE_RELEASE/closure-compiler-v$CLOSURE_RELEASE.jar `
    $root\closure-compiler-v$CLOSURE_RELEASE.jar
Copy-File $root\closure-compiler-v$CLOSURE_RELEASE.jar $root\lib\compiler.jar
Delete-File $root\closure-compiler-v$CLOSURE_RELEASE.jar

Write-Host "Fetching tools.reader $TREADER_RELEASE ..."
Get-WebResource `
    https://repo1.maven.org/maven2/org/clojure/tools.reader/$TREADER_RELEASE/tools.reader-$TREADER_RELEASE.jar `
    $root\lib\tools.reader-$TREADER_RELEASE.jar

Write-Host "Fetching test.check $TEST_CHECK_RELEASE ..."
Get-WebResource `
    https://repo1.maven.org/maven2/org/clojure/test.check/$TEST_CHECK_RELEASE/test.check-$TEST_CHECK_RELEASE.jar `
    $root\lib\test.check-$TEST_CHECK_RELEASE.jar

Write-Host "[Bootstrap Completed]"
