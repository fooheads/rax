(ns fooheads.rax.tree-test
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.rax.tree :as tree]
    [fooheads.setish :as set]
    [fooheads.tbl :refer [tbl]]
    [fooheads.test :refer [thrown-ex-data]]))


(def rel-artist
  (tbl
    | :artist/artist-id   | :artist/name            |
    | ------------------- | --------------          |
    | 22                  | "Led Zeppelin"          |
    | 94                  | "Jimi Hendrix"          |))


(def tree-artist
  [{:id 22 :name "Led Zeppelin"}
   {:id 94 :name "Jimi Hendrix"}])


(def rel-artist-album
  (tbl
    | :artist/artist-id   | :artist/name   | :album/album-id   | :album/artist-id   | :album/title           |
    | ------------------- | -------------- | ----------------- | ------------------ | ---------------------- |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                    |
    | 22                  | "Led Zeppelin" | 131               | 22                 | "IV"                   |
    | 94                  | "Jimi Hendrix" | 120               | 94                 | "Are You Experienced?" |))


(def tree-artist-album
  [{:id 22
    :name "Led Zeppelin"
    :albums
    [{:id 132 :title "I"}
     {:id 131 :title "IV"}]}

   {:id 94
    :name "Jimi Hendrix"
    :albums
    [{:id 120 :title "Are You Experienced?"}]}])


(def tree-album-artist
  [{:id 132 :title "I" :artist {:id 22 :name "Led Zeppelin"}}
   {:id 131 :title "IV" :artist {:id 22 :name "Led Zeppelin"}}
   {:id 120 :title "Are You Experienced?" :artist {:id 94 :name "Jimi Hendrix"}}])


(def rel-artist-album-one-album-without-1-artist
  (tbl
    | :artist/artist-id   | :artist/name   | :album/album-id   | :album/artist-id   | :album/title              |
    | ------------------- | -------------- | ----------------- | ------------------ | ----------------------    |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                       |
    | 22                  | "Led Zeppelin" | 131               | 22                 | "IV"                      |
    | 94                  | "Jimi Hendrix" | 120               | 94                 | "Are You Experienced?"    |
    |                     |                | 666               |                    | "The Number of the Beast" |))

(def tree-album-artist-without-1-artist
  [{:id 132 :title "I" :artist {:id 22 :name "Led Zeppelin"}}
   {:id 131 :title "IV" :artist {:id 22 :name "Led Zeppelin"}}
   {:id 120 :title "Are You Experienced?" :artist {:id 94 :name "Jimi Hendrix"}}
   {:id 666 :title "The Number of the Beast" :artist nil}])


(def rel-single-artist-album
  (tbl
    | :artist/artist-id   | :artist/name   | :album/album-id   | :album/artist-id   | :album/title           |
    | ------------------- | -------------- | ----------------- | ------------------ | ---------------------- |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                    |
    | 22                  | "Led Zeppelin" | 131               | 22                 | "IV"                   |))


(def tree-single-artist-album
  {:id 22
   :name "Led Zeppelin"
   :albums
   [{:id 132 :title "I"}
    {:id 131 :title "IV"}]})


(def rel-artist-album-with-error
  (tbl
    | :artist/artist-id   | :artist/name   | :album/album-id   | :album/artist-id   | :album/title           |
    | ------------------- | -------------- | ----------------- | ------------------ | ---------------------- |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                    |
    | 94                  | "Jimi Hendrix" | 132               | 22                 | "I"                    |))


(def rel-artist-album-track
  (tbl
    | :artist/artist-id   | :artist/name   | :album/album-id   | :album/artist-id   | :album/title           | :track/track-id   | :track/name            |
    | ------------------- | -------------- | ----------------- | ------------------ | ---------------------- | ----------------- | ---------------------- |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                    | 1618              | "Good Times Bad Times" |
    | 22                  | "Led Zeppelin" | 132               | 22                 | "I"                    | 1621              | "Dazed and Confused"   |
    | 22                  | "Led Zeppelin" | 131               | 22                 | "IV"                   | 1610              | "Black Dog"            |
    | 22                  | "Led Zeppelin" | 131               | 22                 | "IV"                   | 1611              | "Rock & Roll"          |
    | 94                  | "Jimi Hendrix" | 120               | 94                 | "Are You Experienced?" | 1480              | "Manic Depression"     |
    | 94                  | "Jimi Hendrix" | 120               | 94                 | "Are You Experienced?" | 1492              | "Purple Haze"          |))


(def tree-artist-album-track
  [{:id 22
    :name "Led Zeppelin"
    :albums
    [{:id 132
      :title "I"
      :tracks
      [{:title "Good Times Bad Times"}
       {:title "Dazed and Confused"}]}
     {:id 131
      :title "IV"
      :tracks
      [{:title "Black Dog"}
       {:title "Rock & Roll"}]}]}

   {:id 94
    :name "Jimi Hendrix"
    :albums
    [{:id 120
      :title "Are You Experienced?"
      :tracks
      [{:title "Manic Depression"}
       {:title "Purple Haze"}]}]}])


(def tree-track-album-and-artist
  [{:title "Good Times Bad Times" :album {:title "I"}                    :artist {:name "Led Zeppelin"}}
   {:title "Dazed and Confused"   :album {:title "I"}                    :artist {:name "Led Zeppelin"}}
   {:title "Black Dog"            :album {:title "IV"}                   :artist {:name "Led Zeppelin"}}
   {:title "Rock & Roll"          :album {:title "IV"}                   :artist {:name "Led Zeppelin"}}
   {:title "Manic Depression"     :album {:title "Are You Experienced?"} :artist {:name "Jimi Hendrix"}}
   {:title "Purple Haze"          :album {:title "Are You Experienced?"} :artist {:name "Jimi Hendrix"}}])


(deftest rel->tree-test
  (testing "list of non-nested map"
    (is (= tree-artist
           (tree/rel->tree
             rel-artist
             [{:id :artist/artist-id :name :artist/name}]))))


  (testing "list of 1-1"
    (is (= tree-album-artist
           (tree/rel->tree
             rel-artist-album
             [{:id :album/album-id
               :title :album/title
               :artist
               {:id :artist/artist-id
                :name :artist/name}}]))))

  (testing "list of 1-1 with a 0-1 (nil)"
    (is (= tree-album-artist-without-1-artist
           (tree/rel->tree
             rel-artist-album-one-album-without-1-artist
             [{:id :album/album-id
               :title :album/title
               :artist
               {:id :artist/artist-id
                :name :artist/name}}]))))

  (testing "nested"
    (is (= tree-artist-album
           (tree/rel->tree
             rel-artist-album
             [{:id :artist/artist-id
               :name :artist/name
               :albums
               [{:id :album/album-id
                 :title :album/title}]}])

           #_(tree/rel->tree
               artist
               [{:id :artist/artist-id
                 :name :artist/name
                 :albums
                 [{:id :album/album-id
                   :title :album/title}]}])))

    (is (= tree-artist-album-track
           (tree/rel->tree
             rel-artist-album-track
             [{:id :artist/artist-id
               :name :artist/name
               :albums
               [{:id :album/album-id
                 :title :album/title
                 :tracks
                 [{:title :track/name}]}]}]))))

  (testing "1-1"
    (testing "top-level 1-1"
      (is (= tree-single-artist-album
             (tree/rel->tree
               rel-single-artist-album
               {:id :artist/artist-id
                :name :artist/name
                :albums
                [{:id :album/album-id
                  :title :album/title}]}))))

    (testing "multiple 1-1 attrs"
      (is (= tree-track-album-and-artist
             (tree/rel->tree
               rel-artist-album-track
               [{:title :track/name
                 :artist {:name :artist/name}
                 :album {:title :album/title}}]))))


    (testing "error"
      (is (= {:guard/msg "mapping requires 0..1, but data contains more than 1"}
             (thrown-ex-data
               [:guard/msg]
               (tree/rel->tree
                 rel-artist-album-with-error
                 [{:id :album/album-id
                   :title :album/title
                   :artist
                   {:id :artist/artist-id
                    :name :artist/name}}])))))))


(deftest tree->rel-test
  (testing "list of non-nested map"
    (is (= rel-artist
           (tree/tree->rel
             tree-artist
             [{:id :artist/artist-id :name :artist/name}]))))

  (testing "list of 1-1"
    (is (= (set/project
             rel-artist-album
             [:artist/artist-id :artist/name :album/album-id :album/title])

           (tree/tree->rel
             tree-album-artist
             [{:id :album/album-id
               :title :album/title
               :artist
               {:id :artist/artist-id
                :name :artist/name}}]))))

  (testing "list of 1-1 with a 0-1 (nil)"
    (is (= (set/project
             rel-artist-album-one-album-without-1-artist
             [:artist/artist-id :artist/name :album/album-id :album/title])

           (tree/tree->rel
             tree-album-artist-without-1-artist
             [{:id :album/album-id
               :title :album/title
               :artist
               {:id :artist/artist-id
                :name :artist/name}}]))))


  (testing "nested"
    (is (= (set/project
             rel-artist-album
             [:artist/artist-id :artist/name :album/album-id :album/title])

           (tree/tree->rel
             tree-artist-album
             [{:id :artist/artist-id
               :name :artist/name
               :albums
               [{:id :album/album-id
                 :title :album/title}]}])))

    (is (= (set/project
             rel-artist-album
             [:artist/artist-id :artist/name :album/album-id :album/title])

           (tree/tree->rel
             tree-artist-album-track
             [{:id :artist/artist-id
               :name :artist/name
               :albums
               [{:id :album/album-id
                 :title :album/title}]}]))))


  (testing "1-1"
    (testing "top-level 1-1"
      (is (= (set/project
               rel-single-artist-album
               [:artist/artist-id :artist/name :album/album-id :album/title])
             (tree/tree->rel
               tree-single-artist-album
               {:id :artist/artist-id
                :name :artist/name
                :albums
                [{:id :album/album-id
                  :title :album/title}]}))))

    (testing "multiple 1-1 attrs"
      (is (= (set/project
               rel-artist-album-track
               [:artist/name
                :album/title
                :track/name])
             (tree/tree->rel
               tree-track-album-and-artist
               [{:title :track/name
                 :artist {:name :artist/name}
                 :album {:title :album/title}}])))))

  (testing "mixing 0..1 and 0..*"
    (is (= [{:track/track-id 100
             :track/name "Good Times"
             :album/title "I"
             :artist/name "Led Zeppelin"}]

           (tree/tree->rel
             {:id 100
              :name "Good Times"
              :album {:title "I"}
              :artist {:name "Led Zeppelin"}}
             {:id :track/track-id
              :name :track/name
              :album {:title :album/title}
              :artist {:name :artist/name}})))

    (is (= [{:track/track-id 100
             :track/name "Good Times"
             :album/title "I"
             :artist/name "Led Zeppelin"
             :tag/name :rock}
            {:track/track-id 100
             :track/name "Good Times"
             :album/title "I"
             :artist/name "Led Zeppelin"
             :tag/name :experimental}]

           (tree/tree->rel
             {:id 100
              :name "Good Times"
              :album {:title "I"}
              :artist {:name "Led Zeppelin"}
              :tags [{:name :rock} {:name :experimental}]}
             {:id :track/track-id
              :name :track/name
              :album {:title :album/title}
              :artist {:name :artist/name}
              :tags [{:name :tag/name}]})))))

