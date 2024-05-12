(ns fooheads.rax.tree
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :refer [forv guard]]))


(defn- simple-attrs [mapping]
  (into {} (filter (fn [[_k v]] (keyword? v)) mapping)))


(defn- coll-attrs [mapping]
  (into {} (filter (fn [[_k v]] (coll? v)) mapping)))


(defn- blank-map? [m]
  (not (some some? (vals m))))


(defn- blank-map [ks]
  (zipmap ks (repeat nil)))


(defn rel->tree
  ([rel mapping]
   (rel->tree rel mapping {}))

  ([rel mapping opts]
   (let [opts (merge {:keep-blank-maps false} opts)
         inner-mapping (if (vector? mapping) (first mapping) mapping)
         simple-attrs (simple-attrs inner-mapping)
         coll-attrs (coll-attrs inner-mapping)

         index (set/index rel (vals simple-attrs))
         renames (set/map-invert simple-attrs)

         tree
         (forv [[m tuples] index
                :let [m (set/rename-keys m renames)

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
  [tree mapping]
  (cond
    (vector? tree)
    (vec (mapcat #(tree->rel % (first mapping)) tree))

    :else
    (let [simple-attrs (simple-attrs mapping)
          coll-attrs (coll-attrs mapping)
          m (->
              ;; Start with a blank map, in order to always get all attrs into
              ;; the tuple, even if tree represents a "left-join" and some
              ;; values are nil
              (blank-map (keys simple-attrs))
              (merge tree)
              (set/rename-keys simple-attrs)
              (select-keys (vals simple-attrs)))

           child-attrs-maps (forv [[k mapping] coll-attrs]
                              (tree->rel (get tree k) mapping))

          res
          (if (empty? child-attrs-maps)
            [m]
            (reduce
              (fn [acc child-maps]
                (forv [child-map child-maps
                       a acc]
                  (merge child-map a)))
              (cons [m] child-attrs-maps)))]
      (vec res))))

