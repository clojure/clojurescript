var path = require('path');
var mdeps = require('module-deps');
var nodeResolve = require('resolve');
var browserResolve = require('browser-resolve');

var target = 'CLJS_TARGET';
var filename = path.resolve(__dirname, 'JS_FILE');
var resolver = target === 'nodejs' ? nodeResolve : browserResolve;

var md = mdeps({
  resolve: function(id, parent, cb) {
    // set the basedir properly so we don't try to resolve requires in the Closure
    // Compiler processed `node_modules` folder.
    parent.basedir = parent.filename === filename ? __dirname: path.dirname(parent.filename);

    resolver(id, parent, cb);
  },
  filter: function(id) {
    return !nodeResolve.isCore(id);
}});

var pkgJsons = [];
var deps_files = {};

md.on('package', function (pkg) {
  // we don't want to include the package.json for users' projects
  if (/node_modules/.test(pkg.__dirname)) {
    var pkgJson = {
      file: path.join(pkg.__dirname, 'package.json'),
    };

    if (pkg.name != null) {
      pkgJson.provides = [ pkg.name ];
    }

    if (pkg.main != null) {
      pkgJson.main = path.join(pkg.__dirname, pkg.main);
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

    if (deps_files[pkgJson.main] != null && pkgJson.provides != null) {
      deps_files[pkgJson.main].provides = pkgJson.provides;
    }

    deps_files[pkgJson.file] = { file: pkgJson.file };
  }

  var values = [];
  for (var key in deps_files) {
    var dep = deps_files[key];

    if (dep.provides == null && !/node_modules[/\\][^/\\]*?[/\\]package.json$/.test(dep.file)) {
      var match = dep.file.match(/node_modules[/\\](.*)\.js(on)*$/)

      if (match != null){
        dep.provides = [ match[1] ];
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
