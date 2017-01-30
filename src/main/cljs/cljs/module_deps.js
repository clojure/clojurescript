var path = require('path');
var mdeps = require('module-deps');

var md = mdeps({});
var deps_files = [];

md.on('package', function (pkg) {
  // we don't want to include the package.json for users' projects
  if (/node_modules/.test(pkg.__dirname)) {
    deps_files.push({file: path.join(pkg.__dirname, 'package.json')});
  }
});

md.on('file', function(file) {
  deps_files.push({file: file});
});

md.on('end', function() {
  process.stdout.write(JSON.stringify(deps_files));
});

md.end({
  file: path.resolve(path.join(__dirname, 'JS_FILE'))
});

md.resume();
