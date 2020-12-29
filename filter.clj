(defn heavy-even [num]
  (Thread/sleep 5)
  (even? num))
;; value-f - функция по которой фильтруем, coll - фильтруемый список
(defn p-filter-finite ([pred coll]
   (let [chunk-size (int (Math/ceil (Math/sqrt (count coll)))),
      parts (partition-all chunk-size coll)] ; разбиваем на подмножества
   (->> parts
      (map (fn [coll1]
        (future (doall (filter pred coll1)))))
      (doall)
      (map deref)  ;; получает результаты выполнения фьючеров
      (flatten)))));; собрать списки воедино

(defn p-filter
  ([pred coll]
   (if (empty? coll) '()
     (concat (p-filter-finite pred (take 1000 coll)) (lazy-seq (p-filter pred (drop 1000 coll)))))))

(time (reduce + (take 100 (p-filter heavy-even (range)))))
(time (reduce + (take 100 (filter heavy-even (range)))))




