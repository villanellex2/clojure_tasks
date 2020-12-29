;;;an empty route map
;;;it is enough to use either forward or backward part (they correspond to each other including shared reference to number of tickets)
;;;:forward is a map with route start point names as keys and nested map as values
;;;each nested map has route end point names as keys and route descriptor as values
;;;each route descriptor is a map (structure in fact) of the fixed structure where
;;;:price contains ticket price
;;;and :tickets contains reference to tickets number
;;;:backward has the same structure but start and end points are reverted
(def empty-map
  {:forward  {},
   :backward {}})

(defn route
  "Add a new route (route) to the given route map
   route-map - route map to modify
   from - name (string) of the start point of the route
   to - name (string) of the end point of the route
   price - ticket price
   tickets-num - number of tickets available"
  [route-map from to price tickets-num]
  (let [tickets (ref tickets-num :validator (fn [state] (>= state 0))), ;reference for the number of tickets
        orig-source-desc (or (get-in route-map [:forward from]) {}),
        orig-reverse-dest-desc (or (get-in route-map [:backward to]) {}),
        route-desc {:price   price,                         ;route descriptor
                    :tickets tickets},
        source-desc (assoc orig-source-desc to route-desc),
        reverse-dest-desc (assoc orig-reverse-dest-desc from route-desc)]
    (-> route-map
        (assoc-in [:forward from] source-desc)
        (assoc-in [:backward to] reverse-dest-desc))))

(def inf 1000000)

(defn dijkstra [g src dst]
  (loop [dists (assoc (zipmap (keys g) (repeat inf)) src 0)
         curr src
         ns (keys (get g curr))
         unvisited (apply hash-set (keys g))
         p (zipmap (keys g) (repeat nil))]
    (if (empty? unvisited) 

      ; если нет непосещенных вершин ->
      [(get dists dst), 
       (loop [path [] t dst]
         (if (= t src)
           (cons src path)      ; собираем весь путь проходя из конечной вершины
           (recur (cons t path) (get p t))))] 
      
      ; если есть непосещенная вершина ->
      (if (empty? ns)
        (let [u (disj unvisited curr)
              ; если у нас больше нет доступных вершин из данной вершины->
              ; запускаем алгоритм из следующей вершины
              next-n (first (sort-by #(get dists %) u))] ; мы выбираем следующую вершину с минимальным сохраненным весом
          (recur dists next-n (filter #(some (fn [s] (= s %)) u) (keys (get g next-n))) u p))
        (let [cdst (get dists curr)
              ; если у нас есть доступные вершины из данной вершины - переходим в неё
              idist (get dists (first ns)) ; берем уже вычисленный путь
              sum (+ cdst (get (get g curr) (first ns))) ; вычисляем сумму пути из этой вершины до следующей
              result (if (< sum idist)            
                       (assoc dists (first ns) sum) ; если новый путь короче - сохраняем его длинну
                       dists)                 
              path (if (< sum idist)      
                     (assoc p (first ns) curr)      ; если новый путь короче - сохраняем путь (ну, вершинки)
                     p)] 
          (recur result curr (rest ns) unvisited path))))))

(defn simplify-graph [graph]
  (let [g (get graph :forward)]
    (zipmap    ; создаем map (начало -> конец -> цена проезда)
      (keys g)
      (map #(zipmap (keys %) (map (fn [param1] 
        (if (> (deref (param1 :tickets)) 0) ; проверка существования билета
          (get param1 :price)               ; есть -> закидываем в map цену
          100000)) (vals %))) (vals g)))))  ; нет -> закидываем в map слишком большую цену 

(defn book-tickets
  "Tries to book tickets and decrement appropriate references in route-map atomically
   returns map with either :price (for the whole route) and :path (a list of destination names) keys
          or with :error key that indicates that booking is impossible due to lack of tickets"
  [route-map from to]
  (if (= from to)
    {:path '(), :price 0}
    (let [graph (simplify-graph route-map) ; создаем граф 
          path (dijkstra graph from to)]   ; ну и передаем его Дийкстре, для нахождения кратчайшего пути
        (try
          (dosync
            (loop [p (second path)] 
              (if (< (count p) 3)   ; если для достижения конечной точки нужно перейти по одному ребру
                nil ; -> прерываем цикл
                ; иначе -> бронируем рейс
                (let [r (:tickets (get (get (:forward route-map) (first p)) (second p)))]; берем число билетов
                  (do
                    (alter r dec)           ; уменьшаем чило билетов
                    (recur (next p)))))))   ; затем смотрим на оставшуюся часть пути
          {:path (second path), :price (first path)}
          (catch Exception e {:error e})))))


;;;cities
(def spec1 (-> empty-map
               (route "City1" "Capital" 200 5)
               (route "Capital" "City1" 250 5)
               (route "City2" "Capital" 200 5)
               (route "Capital" "City2" 250 5)
               (route "City3" "Capital" 300 3)
               (route "Capital" "City3" 400 3)
               (route "City1" "Town1_X" 50 2)
               (route "Town1_X" "City1" 150 2)
               (route "Town1_X" "TownX_2" 50 2)
               (route "TownX_2" "Town1_X" 150 2)
               (route "Town1_X" "TownX_2" 50 2)
               (route "TownX_2" "City2" 50 3)
               (route "City2" "TownX_2" 150 3)
               (route "City2" "Town2_3" 50 2)
               (route "Town2_3" "City2" 150 2)
               (route "Town2_3" "City3" 50 3)
               (route "City3" "Town2_3" 150 2)))

(defn booking-future [route-map from to init-delay loop-delay]
  (future
    (Thread/sleep init-delay)
    (loop [bookings []]
      (Thread/sleep loop-delay)
      (let [booking (book-tickets route-map from to)]
        (if (booking :error)
          bookings
          (recur (conj bookings booking)))))))


(defn print-bookings [name ft]
  (println (str name ":") (count ft) "bookings")
  (doseq [booking ft]
    (println "price:" (booking :price) "path:" (booking :path))))

(defn run []
  ;;try to tune timeouts in order to all the customers gain at least one booking
  (let [f1 (booking-future spec1 "City1" "City3" 99 1),
        f2 (booking-future spec1 "City1" "City2" 100 1),
        f3 (booking-future spec1 "City2" "City3" 100 1)]
    (print-bookings "City1->City3:" @f1)
    (print-bookings "City1->City2:" @f2)
    (print-bookings "City2->City3:" @f3)

    ))

(run)
