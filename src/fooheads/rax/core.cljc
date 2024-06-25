(ns fooheads.rax.core
  (:require
    [fooheads.stdlib :refer [map-vals]]))


(defn map->relmap
  "Takes a map of attrs, groups them by ns, and puts them in a relmap.
  Assumes one tuple per relation."
  [m]

  (->>
    m
    (group-by (comp keyword namespace key))
    (map-vals (fn [attrs] #{(into {} attrs)}))))


(defn relmap->map
  "Takes a relmap and creates a flat map with all attrs in the relmap.
  Assumes one tuple per relation."
  [relmap]
  (->>
    relmap
    vals
    (mapcat first)
    (into {})))

