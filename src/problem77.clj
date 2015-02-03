(ns problem77)

(def test-cases
  '[(= (__ ["meat" "mat" "team" "mate" "eat"])
       #{#{"meat" "team" "mate"}})
    (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
       #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
    (= (__ ["blah" "blah"])
       #{})])

(defn- anagram? [w z]
  (= (sort w) (sort z)))

(anagram? "meat" "team")
(anagram? "meat" "meet")

;; first version
;; Custom implementation of (group-by sort <coll>)
(defn find-anagram-v1
  "Return anagrams based on sort"
  [ws]
  (set (filter #(> (count %) 1)
               (vals (reduce #(let [k (sort %2)
                                    s (% k #{})]
                                (assoc % k (conj s %2)))
                             {}
                             ws)))))

(defn find-anagram-v2
  "Return anagrams based on sort and group-by"
  [ws]
  (->> ws
       (group-by sort)
       vals
       (map set)
       (filter #(> (count %) 1))
       set))

(find-anagram-v2 ["veer" "lake" "item" "kale" "mite" "ever" "toto" "toto"])
(group-by sort ["veer" "lake" "item" "kale" "mite" "ever" "toto" "toto"])

(def __
  find-anagram-v2
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
