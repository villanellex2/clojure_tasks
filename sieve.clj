(use 'clojure.test)

(def naturals
  (lazy-seq (cons 2 (map inc naturals))))

(defn remove_sublist [l1 l2] 
  (lazy-seq 
    (if (< (first l1) (first l2)) 
      (cons (first l1) (remove_sublist (rest l1) l2))
      (if (> (first l1) (first l2)) 
        (remove_sublist l1 (rest l2))
        (remove_sublist (rest l1) (rest l2))))))

(defn sieve [l] 
  (lazy-seq
   (cons (first l) 
     (sieve (remove_sublist (rest l) (map (partial * (first l)) naturals))))))

(def primes (lazy-seq (sieve naturals)))

(deftest primes-test
  (is (= 2 (first primes)))
  (is (= 3 (nth primes 1)))
  (is (= 541 (nth primes 99)))
)

(run-tests)

(print (take 100 primes))