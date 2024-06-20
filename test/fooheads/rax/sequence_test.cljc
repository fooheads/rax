(ns fooheads.rax.sequence-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.rax.sequence :as seq]))


(deftest sequence-gen-test
  (testing "without scope"
    (let [next-id (seq/sequence-gen)]
      (is (= 1 (next-id :order/id)))
      (is (= 2 (next-id :order/id)))

      (is (= 1 (next-id :order-line/id)))
      (is (= 2 (next-id :order-line/id)))))

  (testing "with scope"
    (let [next-id (seq/sequence-gen)]
      (is (= 1 (next-id :order-line/seq-num [:order/id 50])))
      (is (= 2 (next-id :order-line/seq-num [:order/id 50])))
      (is (= 1 (next-id :order-line/seq-num [:order/id 51])))))

  (testing "with start state"
    (let [next-id (seq/sequence-gen {:state {:order/id 100}})]
      (is (= 101 (next-id :order/id)))
      (is (= 102 (next-id :order/id)))))

  (testing "with scope and simple start state"
    (let [next-id (seq/sequence-gen
                    {:state
                     {:order-line/seq-num
                      {["John"] 30
                       ["Jane"] 70}}})]
      (is (= 31 (next-id :order-line/seq-num ["John"])))
      (is (= 71 (next-id :order-line/seq-num ["Jane"])))))

  (testing "with scope and compound start state"
    (let [next-id (seq/sequence-gen
                    {:state
                     {:order-line/seq-num
                      {[:order/id 50] 30
                       [:order/id 51] 70}}})]
      (is (= 31 (next-id :order-line/seq-num [:order/id 50])))
      (is (= 71 (next-id :order-line/seq-num [:order/id 51])))))

  (testing "negative"
    (let [next-f (fn [x] (if x (dec x) -1))
          next-id (seq/sequence-gen {:next-f next-f})]
      (is (= -1 (next-id :order/id)))
      (is (= -2 (next-id :order/id)))

      (is (= -1 (next-id :order-line/id)))
      (is (= -2 (next-id :order-line/id))))))

