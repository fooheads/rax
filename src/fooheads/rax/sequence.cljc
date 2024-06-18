(ns fooheads.rax.sequence
  "Functions to handle unique \"database\" sequences (not Clojure seqs),
  auto increments and similar.")


(defn sequence-gen
  "Returns a stateful function that returns auto incremented sequence ids.
  Each returned function carries it's own state.

  The function is called with a simple value or a vector path as
  the sequence 'name', and returns the next integer for that
  sequence. A sequence can also have a scope (see example).

  Sequences starts at 1 by default, but can take a map as the starting state.

  Simple example
  `(def seq-f (sequence-gen))
   (seq-f :order/id)        ;; => 1
   (seq-f :order/id)        ;; => 2
   (seq-f :order-line/id)   ;; => 1`

  Example with scope:
  `(def seq-f (sequence-gen))
   (seq-f :order-line/seq-num \"John Doe\"])    ;; => 1
   (seq-f :order-line/seq-num \"John Doe\"])    ;; => 2`
   (seq-f :order-line/seq-num [:order/id 50])   ;; => 1
   (seq-f :order-line/seq-num [:order/id 50])   ;; => 2
   (seq-f :order-line/seq-num [:order/id 51])   ;; => 1

  Example with start state:
  `(def seq-f (sequence-gen {:order/id 1000))
   (seq-f :order/id)                            ;; => 1001`
  "
  ([]
   (sequence-gen {}))
  ([state]
   (let [inc-f (fn [x] (if x (inc x) 1))
         seqs (atom state)]
     (letfn
       [(gen-next-f
          ([k]
           (gen-next-f k nil))
          ([k scope]
           (let [full-path (if scope [k scope] [k])]
             (get-in
               (swap! seqs update-in full-path inc-f)
               full-path))))]

       gen-next-f))))

