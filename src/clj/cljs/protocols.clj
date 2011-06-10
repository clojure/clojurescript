;; will move to core

;; let's stick with one core namespace
;;   if these names collide, we will make the protocol names uglier
;;   implementers suffer so users can rock
;; not using "persistent" anywhere -- assumed

;; any analogs for Java collection interfaces? not needed
;; TBD: markers: sequential, sorted, reversible

(defprotocol ICounted
  (count [coll] "constant time count"))

(defprotocol IConjoin
  (conj [coll o]))

;; drop count, equiv, cons -> conj on IConjoin
(defprotocol ICollection
  (empty [coll]))

;; renamed IndexedSeq to start an argument
(defprotocol IOrdinal
  (index [coll]))

(defprotocol IIndexed
  (nth [coll n])
  (nth [coll n not-found]))

;; next/rest/more confusion
;; no cons, get conj from IConjoin
(defprotocol ISeq
  (first [coll])
  (next [coll])
  (rest [coll]))

;; lookup? get?
(defprotocol ILookup
  (lookup [o k]))

;; assoc -> assoc-at?
(defprotocol IAssociative
  (contains-key? [coll k])
  (entry-at [coll k])
  (assoc [coll k v]))

;; overlap with IAssociative
;; naming convention: assoc-x? assocx? assoc-ex
(defprotocol IMap
  (assoc-ex [coll k v]) ;; why here, not on Associative?
  (without [coll k]))

;; get -> member
(defprotocol ISet
  (contains? [coll v])
  (disjoin [coll v])
  (member [coll v]))

(defprotocol IStack
  (peek [coll])
  (pop [coll]))

;; vectors are also counted, collection, associative, lookup
(defprotocol IVector
  (assoc-n [coll n val]))

;; CLJ does one interface + marker interface trick.
;; what do we do here?
(defprotocol IDeref
  (deref [o]))

;; don't love IDerefWithTimeout
;;    expirable, expiry?
(defprotocol IDerefWithExpiry
  (expiring-deref [o msec timeout-val]))

(defprotocol IMeta
  (meta [o]))

