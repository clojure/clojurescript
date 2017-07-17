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

    // add provides to files that are not `package.json`s
    if (!/node_modules[/\\](@[^/\\]+?[/\\])?[^/\\]+?[/\\]package\.json$/.test(dep.file)) {
      if (dep.file.indexOf('node_modules') !== -1) {
        var providedModule = dep.file
            .substring(dep.file.lastIndexOf('node_modules'))
            .replace('\\', '/')
            .replace('node_modules/', '');

        dep.provides = dep.provides || [];
        dep.provides.push(providedModule, providedModule.replace(/\.js(on)?$/, ''));

        var indexReplaced = providedModule.replace(/\/index\.js(on)?$/,'');

        if (/\/index\.js(on)?$/.test(providedModule) &&
            dep.provides.indexOf(indexReplaced) === -1) {
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
