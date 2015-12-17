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

(defn find-direction [ant-id [x y]]
 (cond
    (and (> x 0) (> y 0)) "nw"
    (and (< x 0) (> y 0)) "ne"
    (and (< x 0) (< y 0)) "sw"
    (and (> x 0) (< y 0)) "se"
    (< x 0) "e"
    (> x 0) "w"
    (< y 0) "s"
    (> y 0) "n"))

(defn return-to-nest [ant-id [old-x old-y]]
  (let [[x y] (:location (:stat (command ant-id "go" (find-direction ant-id [old-x old-y]))))]
    (if (and (= x 0) (= y 0))
      [x y]
      (return-to-nest ant-id [x y]))))

(loop []
  (let [ant-id (spawn)
      food-location (find-food-se ant-id)]
  (return-to-nest ant-id food-location))
  (recur))
