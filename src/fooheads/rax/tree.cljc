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
  (map
    (fn [tuple]
      (reduce
        (fn [tuple [fk pk]]
          (if (tuple pk)
            (assoc tuple fk (tuple pk))
            tuple))
        tuple
        fk-map))
    rel))


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
  ([tree mapping]
   (tree->rel tree mapping {}))

  ([tree mapping opts]
   (cond
     (vector? tree)
     (vec (mapcat #(tree->rel % (first mapping) opts) tree))

     :else
     (let [simple-attrs (filter-attrs :simple mapping)
           coll-attrs (filter-attrs :coll mapping)
           m (->
               ;; Start with a blank map, in order to always get all attrs into
               ;; the tuple, even if tree represents a "left-join" and some
               ;; values are nil
               (blank-map (keys simple-attrs))
               (merge tree)
               (apply-generators (:generators opts) simple-attrs)
               (set/rename-keys simple-attrs)
               (select-keys (vals simple-attrs)))

            child-attrs-maps (forv [[k mapping] coll-attrs]
                               (tree->rel (get tree k) mapping opts))

           res
           (if (empty? child-attrs-maps)
             [m]
             (reduce
               (fn [acc child-maps]
                 (forv [child-map child-maps
                        a acc]
                   (merge child-map a)))
               (cons [m] child-attrs-maps)))]
       (->
         res
         (introduce-foreign-keys (:fk-map opts))
         vec)))))

