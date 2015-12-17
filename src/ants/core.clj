(ns ants.core
  (:require [clj-http.client :as client]
            [clojure.edn :as edn]
            [org.httpkit.client :as httpkit]))

(def url "http://172.30.249.47:8888")
(defn teamname [] (str "whoSaysYoucAnt-" (rand-int 999)) )

(defn- command [& params]
  (edn/read-string (:body (client/get (str url "/" (clojure.string/join "/" params))))))

(defn- async-command [callback & params]
  (httpkit/get (str url "/" (clojure.string/join "/" params)) {} callback))

(defn join []
  (:id (:stat (command "join" (teamname)))))

(defn spawn [team-id]
  (:id (:stat (command team-id "spawn"))))

(def se-moves
  (flatten (repeat 13 (concat (repeat 25 "e") "s" (repeat 25 "w") "s"))))

(defn- find-food [ant-id [x y] move]
  (let [stat (:stat (command ant-id "go" move))]
    (if (:got-food stat)
      (reduced (:location stat))
      (:location stat))))

(defn find-food-se [ant-id]
  (reduce #(find-food ant-id %1 %2) [0 0] se-moves))

(defn find-direction [ant-id [x y] [dx dy]]
 (cond
    (and (> x dx) (> y dy)) "nw"
    (and (< x dx) (> y dy)) "ne"
    (and (< x dx) (< y dy)) "se"
    (and (> x dx) (< y dy)) "sw"
    (< x dx) "e"
    (> x dx) "w"
    (< y dy) "s"
    (> y dy) "n"))


(defn async-move-to [ant-id position food-position nest-position got-food gen team-id]
  (if (and (< 0 gen) (= position nest-position))
    (async-move-to (spawn team-id) nest-position food-position nest-position false (dec gen) team-id))
  (let  [desired (if got-food nest-position food-position)]
    (async-command (fn [{body :body}]
                   (let [body (edn/read-string (slurp body))
                         got-food (:got-food (:stat body))
                         position (:location (:stat body))]
                     (async-move-to ant-id position food-position nest-position got-food gen team-id))
                   )
                 ant-id "go" (find-direction ant-id position desired))))

(defn run-with-name [x y team-id]
  (let [ ant-ids (map (fn [x] (spawn team-id)) (range 2)) 
        food-location [x y];[2 3];(find-food-se ant-id)
        ]
   (doall (map #(async-move-to % [0 0] food-location [0 0] false 2 team-id) ant-ids)) 
    (loop [] (recur))
    ))

(defn run [x y]
  (run-with-name x y (rand-int 999)))
