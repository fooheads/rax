(ns fooheads.rax.tree
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :refer [forv guard]]))


(defn- simple-attrs
  [mapping]
  (into {} (filter (fn [[_k v]] (keyword? v)) mapping)))


(defn- coll-attrs
  [mapping]
  (into {} (filter (fn [[_k v]] (coll? v)) mapping)))



(defn rel->tree
  ([rel mapping]
   (rel->tree rel mapping {}))

  ([rel mapping opts]
   (let [inner-mapping (if (vector? mapping) (first mapping) mapping)
         simple-attrs (simple-attrs inner-mapping)
         coll-attrs (coll-attrs inner-mapping)

         index (set/index rel (vals simple-attrs))
         renames (set/map-invert simple-attrs)

         tree
         (forv [[m tuples] index
                :let [m (set/rename-keys m renames)

                      coll-maps (forv [[k mapping] coll-attrs]
                                  {k (rel->tree tuples mapping opts)})

                      m (apply merge m coll-maps)]]

           m)]

     (if (map? mapping)
       (do
         (guard #(< (count %) 2)
                tree
                "mapping requires 0..1, but data contains more than 1"
                {:mapping mapping :rel rel :tree tree})
         (first tree))

       tree))))
