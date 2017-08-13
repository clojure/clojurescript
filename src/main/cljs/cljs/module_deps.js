var fs = require('fs');
var path = require('path');
var mdeps = require('@cljs-oss/module-deps');
var nodeResolve = require('resolve');
var konan = require('konan');
var enhancedResolve = require('enhanced-resolve');

var target = 'CLJS_TARGET';
var filename = fs.realpathSync(path.resolve(__dirname, 'JS_FILE'));
var mainFields =
  target === 'nodejs' ? ['module', 'main'] : ['module', 'browser', 'main'];

var resolver = enhancedResolve.create({
  fileSystem: new enhancedResolve.CachedInputFileSystem(
    new enhancedResolve.NodeJsInputFileSystem(),
    4000
  ),
  extensions: ['.js', '.json'],
  mainFields: mainFields,
  moduleExtensions: ['.js', '.json'],
});

var md = mdeps({
  resolve: function(id, parentOpts, cb) {
    // set the basedir properly so we don't try to resolve requires in the Closure
    // Compiler processed `node_modules` folder.
    parentOpts.basedir =
      parentOpts.filename === filename
        ? path.resolve(__dirname)
        : path.dirname(parentOpts.filename);

    resolver(parentOpts.basedir, id, cb);
  },
  filter: function(id) {
    return !(target === 'nodejs' && nodeResolve.isCore(id));
  },
  detect: function(src) {
    var deps = konan(src);

    return deps.strings;
  },
});

function getPackageJsonMainEntry(pkgJson) {
  for (var i = 0; i < mainFields.length; i++) {
    var entry = mainFields[i];

    if (pkgJson[entry] != null) {
      return pkgJson[entry];
    }
  }
  return null;
}

var pkgJsons = [];
var deps_files = {};

md.on('package', function(pkg) {
  // we don't want to include the package.json for users' projects
  if (/node_modules/.test(pkg.__dirname)) {
    var pkgJson = {
      file: path.join(pkg.__dirname, 'package.json'),
    };

    if (pkg.name != null) {
      pkgJson.provides = [pkg.name];
    }

    var pkgJsonMainEntry = getPackageJsonMainEntry(pkg);
    if (pkgJsonMainEntry != null) {
      pkgJson.mainEntry = path.join(pkg.__dirname, pkgJsonMainEntry);
    }

    pkgJsons.push(pkgJson);
  }
});

md.on('file', function(file) {
  deps_files[file] = { file: file };
});

md.on('end', function() {
  for (var i = 0; i < pkgJsons.length; i++) {
    var pkgJson = pkgJsons[i];

    if (deps_files[pkgJson.mainEntry] != null && pkgJson.provides != null) {
      deps_files[pkgJson.mainEntry].provides = pkgJson.provides;
    }

    deps_files[pkgJson.file] = { file: pkgJson.file };
  }

  var values = [];
  for (var key in deps_files) {
    var dep = deps_files[key];

    // add provides to files that are not `package.json`s
    if (
      !/node_modules[/\\](@[^/\\]+?[/\\])?[^/\\]+?[/\\]package\.json$/.test(
        dep.file
      )
    ) {
      if (dep.file.indexOf('node_modules') !== -1) {
        var providedModule = dep.file
          .substring(dep.file.lastIndexOf('node_modules'))
          .replace(/\\/g, '/')
          .replace('node_modules/', '');

        dep.provides = dep.provides || [];
        dep.provides.push(
          providedModule,
          providedModule.replace(/\.js(on)?$/, '')
        );

        var indexReplaced = providedModule.replace(/\/index\.js(on)?$/, '');

        if (
          /\/index\.js(on)?$/.test(providedModule) &&
          dep.provides.indexOf(indexReplaced) === -1
        ) {
          dep.provides.push(indexReplaced);
        }
      }
    }

    values.push(dep);
  }

  process.stdout.write(JSON.stringify(values));
});

md.end({
  file: filename,
});

md.resume();
