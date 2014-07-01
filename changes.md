## 0.0-2234

### Fixes
* CLJS-812: Recur from case statement generated invalid JavaScript
* CLJS-811: use the correct class loader in cljs.js-deps/goog-resource
* fix fns with metadata under advanced compilation
* CLJS-809: dissoc :file metadata introduced by tools.reader 0.8.4
* mark cljs.reader vars as ^:dynamic to avoid compiler warnings