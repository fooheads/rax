(ns fooheads.rax.tree
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :refer [forv guard]]))


(defn- category
  "Defines a category from a mapping value.
  :simple if it's a keyword (something that should be mapped)
  :coll if it's a collection of some sort
  :value if it's neither of the above and somethins that should just pass
         through untouched."
  [x]
  (cond
    (keyword? x) :simple
    (coll? x) :coll
    :else :value))


(defn- attr-category-fn
  "Returns a pred that answers if a map-entry value is of a particular category"
  [c]
  (fn [arg]
    (let [[_k v] arg]
      (= c (category v)))))


(defn- filter-attrs [category mapping]
  (into {} (filter (attr-category-fn category) mapping)))


(defn- blank-map? [m]
  (not (some some? (vals m))))


(defn- blank-map [ks]
  (zipmap ks (repeat nil)))


(defn- apply-generators [m generators mapping]
  (let [generators (or generators {})]
    (reduce-kv
      (fn [m k _v]
        (if-let [gen (generators k)]
          (assoc m k (gen m k (mapping k)))
          m))
      m
      m)))


(defn- introduce-foreign-keys
  [rel fk-map]
  (mapv
    (fn [tuple]
      (reduce
        (fn [tuple [fk pk]]
          (if (tuple pk)
            (assoc tuple fk (tuple pk))
            tuple))
        tuple
        fk-map))
    rel))


(defn- attrs-by-category [mapping]
  {:simple (filter-attrs :simple mapping)
   :coll (filter-attrs :coll mapping)
   :value (filter-attrs :value mapping)})


(defn- rel-keys [mapping]
  (if (vector? mapping)
    (rel-keys (first mapping))

    (let [res
          (let [{:keys [simple coll]} (attrs-by-category mapping)]
            (->>
              coll
              vals
              (map rel-keys)
              (apply concat (vals simple))))]
      res)))



(defn rel->tree
  ([rel mapping]
   (rel->tree rel mapping {}))

  ([rel mapping opts]
   (let [opts (merge {:keep-blank-maps false} opts)
         inner-mapping (if (vector? mapping) (first mapping) mapping)
         simple-attrs (filter-attrs :simple inner-mapping)
         coll-attrs (filter-attrs :coll inner-mapping)
         value-attrs (filter-attrs :value inner-mapping)

         index (set/index rel (vals simple-attrs))
         renames (set/map-invert simple-attrs)

         tree
         (forv [[m tuples] index
                :let [m (set/rename-keys m renames)
                      constants (select-keys inner-mapping (keys value-attrs))
                      m (merge m constants)

                      coll-maps (forv [[k mapping] coll-attrs]
                                  {k (rel->tree tuples mapping opts)})

                      m (apply merge m coll-maps)]

                :when (or (:keep-blank-maps opts)
                          (not (blank-map? m)))]
           m)]

     (if (map? mapping)
       (do
         (guard #(< (count %) 2)
                tree
                "mapping requires 0..1, but data contains more than 1"
                {:mapping mapping :rel rel :tree tree})
         (first tree))

       tree))))



(defn tree->rel
  "Maps a tree into a relation (a list of maps) using a mapping specification.

  A tree is something that represents an aggregate or a list of aggregates,
  where a typical example could be an order and it's orderlines. A common
  place where you will find data of this shape is on web requests, where JSON
  data typically have this shape.

  This function transforms a tree like that into a list of maps (which in
  relational algebra is called a relation); a flat data structure looking like
  a typical result set from a database.

  Once you have the relation, you can continue your transformations
  in the list world, using relational algebra tools like clojure.set or
  fooheads.setish or by just using map/reduce/filter.

  Each tuple (row) in the returned relation will always contains all keys/attrs
  that are specificed in the mapping. If the values does not exist in the tree,
  the resulting values will simply be nil.

  See the tests for extensive examples of how to create mappings and what
  to expect from the transformation:

  https://github.com/fooheads/rax/blob/main/test/fooheads/rax/tree_test.cljc"
  ([tree mapping]
   (tree->rel tree mapping {}))

  ([tree mapping opts]
   ;; (prn "--> -tree->rel" tree mapping)

   (let [res
         (cond

           (map? mapping)
           (cond
             (nil? tree)
             (tree->rel {} mapping)

             (map? tree)
             (let [{simple-attrs :simple coll-attrs :coll} (attrs-by-category mapping)
                   m (->
                       ;; Start with a blank map, in order to always get all
                       ;; attrs/keys specified in the mapping into the tuple,
                       ;; even if some values happens to be non-existent.
                       (blank-map (keys simple-attrs))
                       (merge tree)
                       (apply-generators (:generators opts) simple-attrs)
                       (set/rename-keys simple-attrs)
                       (select-keys (vals simple-attrs)))

                   colls
                   (for [[k mapping] coll-attrs]
                     (let [res (tree->rel (get tree k) mapping opts)]
                       ;; Either the result is a seq, or we introduce a blank
                       ;; map; a map with all keys but with nil values. This
                       ;; is necessary, in order to not loose the whole
                       ;; in the cartesian product if there are no parts.
                       (or (seq res) [(blank-map (rel-keys mapping))])))

                   ;; The resulting rel is the cartiesian product of the whole
                   ;; and all its colls.
                   rel (reduce
                         (fn [xrel yrel]
                           (for [x xrel
                                 y yrel]
                             (merge x y)))
                         [m]
                         colls)]

               (-> rel (introduce-foreign-keys (:fk-map opts)) (vec)))

             :else
             (throw (ex-info "mapping and tree must have the same shape"
                             {:mapping mapping :tree tree})))

           (sequential? mapping)
           (cond
             (nil? tree)
             (tree->rel [] mapping opts)

             (sequential? tree)
             (vec (mapcat #(tree->rel % (first mapping) opts) tree))

             :else
             (throw (ex-info "mapping and tree must have the same shape"
                             {:mapping mapping :tree tree})))

           :else
           (throw (ex-info "unsupported mapping type" {:mapping mapping
                                                       :tree tree})))]
     res)))

