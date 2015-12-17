(ns ants.core
  (:require [clj-http.client :as client]
            [clojure.edn :as edn]))

(def url "http://172.30.249.47:8888")
(def teamname (str "whoSaysYoucAnt-" (rand-int 99999)) )

(defn- command [& params]
  (edn/read-string (:body (client/get (str url "/" (clojure.string/join "/" params))))))

(defn join []
  (:id (:stat (command "join" teamname))))

(def team-id
  (join))

(defn spawn []
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

(defn move-to [ant-id position desired]
  (let [pos (:location (:stat (command ant-id "go" (find-direction ant-id position desired))))]
    (if (= pos desired)
      pos
      (move-to ant-id pos desired))))

(defn- find-and-stash [ant-id food-location]
  (move-to ant-id [0 0] food-location)
  (move-to ant-id food-location [0 0])
  ant-id)

(defn- find-and-spawn [ant-id food-location]
  (find-and-stash ant-id food-location)
  (spawn))

(defn run []
  (let [ant-ids (map (fn [x] (spawn)) (range 5)) 
        food-location [-2 2];(find-food-se ant-id)
        ]
    (loop [find-fn find-and-spawn
           ant-ids ant-ids]
      (let [new-ant-ids (pmap #(find-fn % food-location) ant-ids)
            all-ants (distinct (concat new-ant-ids ant-ids))]
        (if (> 30 (count all-ants))
          (recur find-and-spawn all-ants)
          (recur find-and-stash all-ants))))))
