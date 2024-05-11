(ns fooheads.rax.tree
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :refer [guard]]))


(defn- simple-attrs [mapping]
  (into {} (filter (fn [[_k v]] (keyword? v)) mapping)))


(defn- coll-attrs [mapping]
  (into {} (filter (fn [[_k v]] (coll? v)) mapping)))



(defn rel->tree
  ([rel mapping]
   (rel->tree rel mapping {}))

  ([rel mapping opts]
   (let [inner-mapping (if (vector? mapping) (first mapping) mapping)
         simple-attrs (simple-attrs inner-mapping)
         coll-attrs (coll-attrs inner-mapping)

         ;; An index, where the keys are the simple attrs and the
         ;; values are all tuples matching the key
         index (set/index rel (vals simple-attrs))

         renames (set/map-invert simple-attrs)

         new-rel
         (->>
           (for [[m tuples] index]
             (let [m (set/rename-keys m renames)]
               (if (empty? coll-attrs)
                 m
                 (->>
                   (for [[k mapping] coll-attrs]
                     {k (rel->tree tuples mapping opts)})
                   (apply merge m)))))
           (vec))]


     (if (map? mapping)
       (do
         (guard #(< (count %) 2)
                new-rel
                "mapping requires 0..1, but data contains more than 1"
                {:mapping mapping :rel rel :new-rel new-rel})
         (first new-rel))

       new-rel))))
