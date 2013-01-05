(ns cljs.io)

(def is-node? (if (try js/require (catch js/Error e nil)) true false))
(def node-fs (if is-node? (js/require "fs") nil))
(def node-path (if is-node? (js/require "path") nil))

(defn- file-read
  [path]
  (if is-node?
    (.toString (.readFileSync node-fs path))
    (let [req (doto (js/XMLHttpRequest.)
                    (.open "GET" path false))]
      (.send req) ;; false above mean synchronous/blocking      
      (if (= 200 (.-status req))
        (.-responseText req)
        (throw (js/Error. (str "Could not file-read: " path)))))))

(defn- file-write
  [path data]
  (if is-node?
    (.writeFileSync node-fs path data)
    (throw (js/Error. "No file-write capability defined for this JS environment"))))

