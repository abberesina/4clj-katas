(ns problem105)

(def test-cases
 '[(= {} (__ []))
   (= {:a [1]} (__ [:a 1]))
   (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
   (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))])

(defn part-kv [[k & xs]]
  (if k
    (let [[a b] (split-with number? xs)]
      (assoc (part-kv b) k a))
    {}))

(defn part-kv2 [xs]
  (loop [r {}
         [k & s] xs]
    (if k
      (let [[a b] (split-with number? s)]
        (recur (assoc r k a) b))
      r)))

(defn part-kv3 [kvs]
  (letfn [(f [xs]
            (loop [r []
                   [k v & xs :as xxs] xs]
              (if (seq xxs)
                (if (and (keyword? k) (keyword? v))
                  (recur (-> r (conj k) (conj nil)) (rest xxs))
                  (recur (conj r k) (rest xxs)))
                r)))]
    (into {} (map (fn [[[k] v]]
                    (if (first v)
                      [k v]
                      [k []]))
                  (partition-all 2 (partition-by type (f kvs)))))))

(part-kv3 [:a :b :c :d 1 2 3])
(part-kv [])

(def __
  part-kv2
  )

(defn test-code
  []
  (doseq [[test-case test-number] (map vector test-cases (range))]
    (if (eval `(let [~'__ __]
                 ~test-case))
      (printf "Test #%d passed!\n" (inc test-number))
      (printf "Test #%d failed!\n" (inc test-number)))))
