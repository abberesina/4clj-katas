(ns problem132)

(def test-cases
 '[(= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
   (= '(2) (__ > :more [2]))
   (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
   (empty? (__ > :more ()))])


;; solutions pour comparer 2 valeurs d'une serie,
;; 1/ 2 sequences s & (next s)
;; 2/ fonction de mapping avec destruct qui va au-dela de la fin + partition
;; 3/ partition
;; 4/ recur avec destruct
(defn insert-with1 [p v c]
  (mapcat (fn [a b]
            (cond (= b \-) [a]
                  (p a b) [a v]
                  :x [a]))
          c `(~@(next c) \-)))

(insert-with1 < :zzz [1 3 4 2 18 12 7])

(defn insert-with2 [p v c]
  (mapcat (fn [[a b]]
            (if (and b (p a b))
              [a v]
              [a]))
          (partition-all 2 1 c))) ;; return a partition with last element

(insert-with2 < :zzz [1 3 4 2 18 12 7])

(defn insert-with3 [p v c]
  (let [[x & y] (map (fn [[a b]]
                       (if (p a b)
                         `(~a ~v ~b)
                         `(~a ~b)))
                     (partition 2 1 c))]
    (if y
      `(~@x ~@(mapcat #(drop 1 %) y))
      c)))

(defn insert-with4 [p v c]
  (loop [r [] [x y & m] c]
    (let [z (partial conj r x)]
      (if y
        (if (p x y)
          (recur (z v) `(~y ~@m))
          (recur (z) `(~y ~@m)))
        (z)))))

(defn insert-with4b [p v c]
  (loop [r [] [x y & m :as a] c]
    (let [z (partial conj r x)]
      (if y
        (if (p x y)
          (recur (z v) (rest a))
          (recur (z) (rest a)))
        (z)))))


(insert-with4 < :zzz [1 3 4 2 18 12 7])
(insert-with4 < :zzz [1])
(insert-with4 < :zzz ())


(def __
  insert-with3
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
