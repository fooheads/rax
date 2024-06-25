(ns fooheads.rax.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [fooheads.rax.core :as rax]))


(def simple-relmap
  {:album
   #{{:album/album-id 132 :album/artist-id 22 :album/title "I"}}

   :artist
   #{{:artist/artist-id 22 :artist/name "Led Zeppelin"}}

   :track
   #{{:track/name "Good Times Bad Times" :track/track-id 1618}}})


(def simple-map
  {:track/track-id 1618
   :track/name "Good Times Bad Times"
   :album/album-id 132
   :album/artist-id 22
   :album/title "I"
   :artist/artist-id 22
   :artist/name "Led Zeppelin"})


(deftest map->relmap-test
  (is (= simple-relmap (rax/map->relmap simple-map))))


(deftest relmap->map-test
  (is (= simple-map (rax/relmap->map simple-relmap))))

