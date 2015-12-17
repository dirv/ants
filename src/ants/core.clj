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

(defn look [ant-id]
  (command ant-id "look"))

(defn- has-food? [[d v]]
  (< 0 (count (filter #(= :food (:type %)) v))))

(defn neighbour-with-food [ant-id]
  (let [neighbours (filter has-food? (:surroundings (:stat (look ant-id))))]
    (if (not (empty? neighbours))
      (first (first neighbours)))))

(defn join []
  (:id (:stat (command "join" (teamname)))))

(defn try-spawn [team-id]
  ;(Thread/sleep (* i 100))
  (try
    (:id (:stat (command team-id "spawn")))
    (catch Exception e
      (println e)
      nil)))

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

(def nw-weight ["nw" "nw" "nw" "n" "n" "w" "w" "s" "e" "se" "ne"])
(def sw-weight ["sw" "sw" "sw" "s" "s" "w" "w" "n" "e" "se" "ne"])
(def ne-weight ["ne" "ne" "ne" "n" "n" "e" "e" "s" "w" "sw" "se"])
(def se-weight ["se" "se" "se" "s" "s" "e" "e" "n" "w" "nw" "sw"])

(defn- visited? [seen [x y] direction]
  (let [new-position (case direction
                       "nw" [(dec x) (dec y)]
                       "ne" [(inc x) (dec y)]
                       "sw" [(dec x) (inc y)]
                       "se" [(inc x) (dec y)]
                       "n" [x (dec y)]
                       "s" [x (inc y)]
                       "e" [(inc x) y]
                       "w" [(dec x) y])]
    (some #{new-position} seen)))

(defn- choose-next [ant-id position weighting seen]
  (or (neighbour-with-food ant-id)
      (rand-nth (remove (partial visited? seen position) weighting))))

(def nest-position [0 0])

(defn async-move-to [ant-id position got-food team-id weighting seen]
  (async-command (fn [{body :body error :error}]
                   (if error (println "ERROR" error))
                   (let [body (edn/read-string (slurp body))
                         got-food (:got-food (:stat body))
                         position (:location (:stat body))]
                     (async-move-to ant-id position got-food team-id weighting (conj seen position))))
                 ant-id "go" (if got-food
                               (find-direction ant-id position nest-position)
                               (choose-next ant-id position weighting seen))))

(defn start-spawn [team-id weighting]
  (if-let [ant-id (try-spawn team-id)]
    (async-move-to ant-id [0 0] false team-id weighting [])))

(defn run-with-name [team-id]
  (let [ant-ids (vec (remove nil? (map (partial start-spawn team-id) [nw-weight sw-weight ne-weight se-weight se-weight]))) 
        ]
    (loop [] (recur))
    ))


