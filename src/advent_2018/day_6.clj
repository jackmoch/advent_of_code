(ns advent-2018.day-6
  (:require [clojure.string :as str]
            [criterium.core :as crit]))

(def toy-data "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9")
(def data "77, 279\n216, 187\n72, 301\n183, 82\n57, 170\n46, 335\n55, 89\n71, 114\n313, 358\n82, 88\n78, 136\n339, 314\n156, 281\n260, 288\n125, 249\n150, 130\n210, 271\n190, 258\n73, 287\n187, 332\n283, 353\n66, 158\n108, 97\n237, 278\n243, 160\n61, 52\n353, 107\n260, 184\n234, 321\n181, 270\n104, 84\n290, 109\n193, 342\n43, 294\n134, 211\n50, 129\n92, 112\n309, 130\n291, 170\n89, 204\n186, 177\n286, 302\n188, 145\n40, 52\n254, 292\n270, 287\n238, 216\n299, 184\n141, 264\n117, 129")

(defn parse [s]
  (map (fn [l]
         (mapv #(Integer/parseInt %)
               (str/split l #", ")))
       (str/split-lines s)))

(defn manhattan-distance [[x0 y0] [x1 y1]]
  (+ (Math/abs (- y1 y0))
     (Math/abs (- x1 x0))))

(defn find-nearest-coord [current-loc all-locs]
  (let [ret (->> all-locs
                 (reduce #(update %1 (manhattan-distance current-loc %2) conj %2) {})
                 (apply min-key key))]
    (if (= (count (val ret)) 1)
      {:owner (first (val ret))
       :loc   current-loc})))

(defn part-1 []
  (let [d (parse data)
        max-x (apply max (map first d))
        max-y (apply max (map second d))
        board (for [x (range 0 (+ max-x 1))
                    y (range 0 (+ max-y 1))]
                [x y])
        coord-touches-edge? (fn [[_ points]]
                              (some (fn [{:keys [loc]}]
                                      (let [[x y] loc]
                                        (or (= x 0)
                                            (= y 0)
                                            (>= x max-x)
                                            (>= y max-y))))
                                    points))]
    (->> (repeat d)
         (map find-nearest-coord board)
         (remove nil?)
         (group-by :owner)
         (remove coord-touches-edge?)
         (map (comp count second))
         (apply max))))

(defn part-2 []
  (let [d (parse data)
        max-x (apply max (map first d))
        max-y (apply max (map second d))
        board (for [x (range 0 (+ max-x 1))
                    y (range 0 (+ max-y 1))]
                [x y])
        total-distance (fn [point all-coords]
                         (reduce #(+ %1 (manhattan-distance point %2)) 0 all-coords))]
    (->> (repeat d)
         (map total-distance board)
         (filter #(> 10000 %))
         count)))

(comment (part-1)
         (part-2))