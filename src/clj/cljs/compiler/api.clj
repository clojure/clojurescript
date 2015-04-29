;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software

(ns cljs.compiler.api
  "This is intended to be a stable api for those who intend to create
  tools that use compiler data.

  For example: a build script may need to how to invalidate compiled
  files so that they will be recompiled."
  (:require [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]))
