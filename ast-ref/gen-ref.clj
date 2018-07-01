(use 'clojure.string)

(def tej-ref (read-string (slurp "ast-ref.edn")))
(def html (slurp "quickref.html.tpl"))

(defn fix [x]
  (-> (str x)
    (replace #"`(.*?)`" "<code>$1</code>")
    (replace #":([a-zA-Z\?!\-]*)" "<code>:$1</code>")))

(defn build-children [children]
  (if (some #(:optional (meta %)) children)
    (let [[c & rest] children]
      (let [k (build-children rest)
            kc (mapv (fn [x] (cons c x)) k)]
        (if (:optional (meta c))
          (into k kc)
          kc)))
    (if (seq children)
      [children]
      [[]])))

(defn children [keys]
  (when-let [children (seq (filter #(:children (meta %)) keys))]
    (mapv #(mapv first %) (build-children children))))

(def nodes
  (apply str (for [{:keys [op doc keys]} (:node-keys tej-ref) :let [op (name op)]]
               (str "<section>"
                    "<h2>" "<a href=\"#" op "\" name=\"" op "\">#</a>" op "</h2>"
                    "<h4>" doc "</h4>"
                    "<dl>"
                    "<dt> :op </dt> <dd> <code>:" op "</code></dd>"
                    (apply str (for [[k d :as f] keys]
                                 (str "<dt>" k "</dt>"
                                      "<dd>" (if (:optional (meta f))
                                               "<b>optional</b> ") (fix d) "</dd>")))
                    (if-let [c (children keys)]
                      (str "<dt> :children </dt> <dd> "
                           (join ", " (mapv (fn [c] (str "<code>" c "</code>")) c)) "</dd>"))
                    "</dl>"
                    "</section>\n"))))

(def nav
  (apply str (for [{op :op} (:node-keys tej-ref) :let [op (name op)]]
               (str "<li><a href=\"#" op "\">" op "</a></li>\n"))))

(def common
  (apply str (str "<section>"
                  "<dl>"
                  (apply str (for [[k d :as f] (:all-keys tej-ref)]
                               (str "<dt>" k "</dt>"
                                    "<dd>" (if (:optional (meta f))
                                             "<b>optional</b> ") (fix d) "</dd>")))
                  "</dl>"
                  "</section>\n")))

(spit "quickref.html"
      (-> html
        (replace "{nav}" nav)
        (replace "{common}" common)
        (replace "{nodes}" nodes)))
