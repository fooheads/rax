(ns fooheads.rax.tree
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :refer [guard]]))


(defn- simple-attrs [mapping]
  (into {} (filter (fn [[_k v]] (keyword? v)) mapping)))


(defn- coll-attrs [mapping]
  (into {} (filter (fn [[_k v]] (coll? v)) mapping)))


(defn rel->tree [rel mapping]
  (let [inner-mapping (if (vector? mapping) (first mapping) mapping)
        simple-attrs (simple-attrs inner-mapping)
        coll-attrs (coll-attrs inner-mapping)

        ;; Create an index, so that the keys are the simple attrs and the
        ;; values are the complete tuples for each key
        index (set/index rel (vals simple-attrs))

        renames (set/map-invert simple-attrs)

        new-rel
        (->>
          index
          (reduce
            (fn [acc [m tuples]]
              (let [renamed-m (set/rename-keys m renames)]
                (if-not (seq coll-attrs)
                  ;; No collections, just a simple top-level map
                  (conj acc [renamed-m])

                  ;; Handle each collection attribute
                  (let [res
                        (reduce
                          (fn [acc-xs [k k-mappings]]
                            (let [sub-mapping k-mappings
                                  sub-m {k (rel->tree tuples sub-mapping)}]
                              (conj acc-xs (merge renamed-m sub-m))))
                          []
                          coll-attrs)]

                    ;; Merge all collection attrs
                    (conj acc (apply merge res))))))
            [])
          (flatten)
          (vec))]

      (if (map? mapping)
        (do
          (guard #(= 1 (count %)) new-rel "mapping requires 1-1, but data does not contain exactly 1"
                 {:mapping mapping :rel rel :new-rel new-rel})
          (first new-rel))

        new-rel)))


