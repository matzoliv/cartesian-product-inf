(ns cartesian-product-inf.core
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(defn cartesian-product-by-rank [& seqs-init]
  (let [gen-for-rank
        (fn [elems-at-rank rank]
          (->>
           ;; compute all the permutations of pinned / unpinned sequences
           (apply cartesian-product
                  (map (fn [elem-at-rank seq-init]
                         [(take rank seq-init) ;; unpinned sequence
                          [elem-at-rank]]) ;; pinned sequence
                       elems-at-rank
                       seqs-init))
           ;; drop the first combination of all unpinned sequences
           ;; which would lead to produce all elements of previous ranks.
           (rest)
           ;; use those as successive arguments to a cartesian product
           ;; and concatenate everything
           (mapcat (partial apply cartesian-product))))
        ;; iterate all ranks
        gen-all
        (fn gen-all [seqs rank]
          (and (some seq seqs)
               (cons (gen-for-rank (map first seqs) rank)
                     (lazy-seq (gen-all (map rest seqs) (inc rank))))))]
    (cons
     ;; the very first element
     (map first seqs-init)
     ;; plus the concatenation of all ranks
     (apply concat (gen-all (map rest seqs-init) 1)))))

