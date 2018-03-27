// NOTE: This code should only employ single quotes for strings.
// If double quotes are used, then when the contents of this file
// are passed to node via --eval on Windows, the double quotes
// will be elided, leading to syntactically incorrect JavaScript.

let fs = require('fs');
let path = require('path');
let mdeps = require('@cljs-oss/module-deps');
let nodeResolve = require('resolve');
let babylon = require('babylon');
let traverse = require('babel-traverse').default;
let enhancedResolve = require('enhanced-resolve');

let target = 'CLJS_TARGET';
let filename = fs.realpathSync(path.resolve(__dirname, 'JS_FILE'));
let mainFields = MAIN_ENTRIES;
let aliasFields = target === 'nodejs' ? [] : ['browser'];

// https://github.com/egoist/konan
let getDeps = function (src, {dynamicImport = true, parse = {sourceType: 'module', plugins: '*'}} = {}) {
    const modules = {strings: [], expressions: []};

    let ast;

    if (typeof src === 'string') {
        const moduleRe = /\b(require|import|export)\b/;

        if (!moduleRe.test(src)) {
            return modules;
        }

        ast = babylon.parse(src, parse);
    } else {
        ast = src;
    }

    traverse(ast, {
        enter(path) {
            if (path.node.type === 'CallExpression') {
                const callee = path.get('callee');
                const isDynamicImport = dynamicImport && callee.isImport();
                if (callee.isIdentifier({name: 'require'}) || isDynamicImport) {
                    const arg = path.node.arguments[0];
                    if (arg.type === 'StringLiteral') {
                        modules.strings.push(arg.value);
                    } else {
                        modules.expressions.push(src.slice(arg.start, arg.end));
                    }
                }
            } else if (path.node.type === 'ImportDeclaration' ||
                       path.node.type === 'ExportNamedDeclaration' ||
                       path.node.type === 'ExportAllDeclaration') {
                const source = path.node.source;

                if (source != null) {
                    modules.strings.push(path.node.source.value);
                }
            }
        }
    });

    return modules;
};

let resolver = enhancedResolve.create({
    fileSystem: new enhancedResolve.CachedInputFileSystem(
        new enhancedResolve.NodeJsInputFileSystem(),
        4000
    ),
    extensions: ['.js', '.json'],
    mainFields: mainFields,
    aliasFields: target === 'nodejs' ? [] : ['browser'],
    moduleExtensions: ['.js', '.json'],
    symlinks: false
});

let md = mdeps({
    resolve: function (id, parentOpts, cb) {
        // set the basedir properly so we don't try to resolve requires in the Closure
        // Compiler processed `node_modules` folder.
        parentOpts.basedir =
            parentOpts.filename === filename
                ? path.resolve(__dirname)
                : path.dirname(parentOpts.filename);

        resolver(parentOpts.basedir, id, cb);
    },
    filter: function (id) {
        return !(target === 'nodejs' && nodeResolve.isCore(id)) &&
            !id.startsWith('goog:');
    },
    detect: function (src) {
        let deps = getDeps(src);

        return deps.strings;
    }
});

function getPackageJsonMainEntry(pkgJson) {
    for (let i = 0; i < mainFields.length; i++) {
        let entry = mainFields[i];
        const entryVal = pkgJson[entry];

        if (entryVal != null) {
          if (typeof entryVal === 'string') {
            return entryVal;
          } else if (typeof entryVal === 'object') {
            for (let j = i; j < mainFields.length; j++) {
              let otherEntry = mainFields[j];
              const otherEntryVal = pkgJson[entry];

              if (entryVal[otherEntryVal] != null) {
                return entryVal[otherEntryVal]
              }
            }
          }
        }
    }
    return null;
}

function depProvides(provides, file) {
  const result = provides != null ? provides.slice(0) : [];

  let providedModule = file
      .substring(file.lastIndexOf('node_modules'))
      .replace(/\\/g, '/')
      .replace('node_modules/', '');

  result.push(
    providedModule,
    providedModule.replace(/\.js(on)?$/, '')
  );

  let indexReplaced = providedModule.replace(/\/index\.js(on)?$/, '');

  if (
      /\/index\.js(on)?$/.test(providedModule) &&
      result.indexOf(indexReplaced) === -1
  ) {
    result.push(indexReplaced);
  }

  return result;
}

let pkgJsons = [];
let deps_files = {};

md.on('package', function (pkg) {
    // we don't want to include the package.json for users' projects
    if (/node_modules/.test(pkg.__dirname)) {
        let pkgJson = {
            basedir: pkg.__dirname,
            file: path.join(pkg.__dirname, 'package.json'),
        };

        if (pkg.name != null) {
            pkgJson.provides = [pkg.name];
        }

        let pkgJsonMainEntry = getPackageJsonMainEntry(pkg);
        if (pkgJsonMainEntry != null) {
            pkgJson.mainEntry = path.join(pkg.__dirname, pkgJsonMainEntry);
        }

        // we'll need these later
        for (let i = 0; i < aliasFields.length; i++) {
          const field = aliasFields[i];
          if (pkg[field] != null) {
            pkgJson[field] = pkg[field];
          }
        }

        pkgJsons.push(pkgJson);
    }
});

md.on('file', function (file) {
    deps_files[file] = {file: file};
});

md.on('end', function () {
    for (let i = 0; i < pkgJsons.length; i++) {
        let pkgJson = pkgJsons[i];
        const candidates = /\.js(on)?$/.test(pkgJson.mainEntry)
            ? [pkgJson.mainEntry]
            : [pkgJson.mainEntry, pkgJson.mainEntry + '.js', pkgJson.mainEntry + '/index.js', pkgJson.mainEntry + '.json'];

        for (let j = 0; j < candidates.length; j++) {
          const candidate = candidates[j];

          if (deps_files[candidate] != null && pkgJson.provides != null) {
            deps_files[candidate].provides = pkgJson.provides;
          }
        }

        for (let j = 0; j < aliasFields.length; j++) {
          const field = aliasFields[j];
          const fieldValue = pkgJson[field];

          if (fieldValue != null && typeof fieldValue === 'object') {
            for (let key in fieldValue) {
              const replacement = path.resolve(pkgJson.basedir, fieldValue[key]);

              if (deps_files[replacement] != null) {
                const file = path.resolve(pkgJson.basedir, key);
                deps_files[replacement].provides = depProvides(deps_files[replacement].provides, file);

                if (file === pkgJson.mainEntry) {
                  Array.prototype.push.apply(deps_files[replacement].provides, pkgJson.provides);
                }
              }
            }
          }
        }


        deps_files[pkgJson.file] = {file: pkgJson.file};
    }

    let values = [];
    for (let key in deps_files) {
        let dep = deps_files[key];

        // add provides to files that are not `package.json`s
        if (
            !/node_modules[/\\](@[^/\\]+?[/\\])?[^/\\]+?[/\\]package\.json$/.test(
                dep.file
            )
        ) {
            if (dep.file.indexOf('node_modules') !== -1) {
              dep.provides = depProvides(dep.provides, dep.file);
            }
        }

        values.push(dep);
    }

    process.stdout.write(JSON.stringify(values));
});

md.end({
    file: filename
});

md.resume();
