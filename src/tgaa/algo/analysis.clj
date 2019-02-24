(ns tgaa.algo.analysis
    (:require [tgaa.struct.shared :as shared]
             [tgaa.struct.ant :as ant] 
             [tgaa.struct.image :as image])
    (:import [tgaa.hull ConvexHull Point]))

(def box-sens 100)

(defn is-boxed [path-1 path-2]
  (let [[start-x-1 start-y-1] (ant/ant-start-point path-1)
        [end-x-1 end-y-1] (ant/ant-end-point path-1)
        [start-x-2 start-y-2] (ant/ant-start-point path-2)
        [end-x-2 end-y-2] (ant/ant-end-point path-2)
        [leftmost-x-1 rightmost-x-1] (if (< start-x-1 end-x-1) [start-x-1 end-x-1] [end-x-1 start-x-1])
        [highest-y-1 lowest-y-1] (if (< start-y-1 end-y-1) [start-y-1 end-y-1] [end-y-1 start-y-1])
        [leftmost-x-2 rightmost-x-2] (if (< start-x-2 end-x-2) [start-x-2 end-x-2] [end-x-2 start-x-2])
        [highest-y-2 lowest-y-2] (if (< start-y-2 end-y-2) [start-y-2 end-y-2] [end-y-2 start-y-2])]
    (and (<= (+ leftmost-x-1  box-sens) rightmost-x-2)
         (>= (- rightmost-x-1 box-sens) leftmost-x-2)
         (<= (- highest-y-1 box-sens) lowest-y-2)
         (>= (+ lowest-y-1 box-sens) highest-y-2))))

(defn derive-slope [ap]
  (let [[x y] (map #(- %1 %2) (ant/ant-start-point ap) (ant/ant-end-point ap))]
    (if (and (= x 0) (= y 0))
      (do (print "Invalid Slope") "fail")
      (if (= 0 x) 
        "inf"
        (/ y x)))))

(defn formula-ref-point [ap]
  (if (< (first (ant/ant-start-point ap)) (first (ant/ant-end-point ap)))
    (ant/ant-start-point ap) 
    (ant/ant-end-point ap)))

(defn slp-int-formula [ap]
  (let [slp (derive-slope ap)
        [x c] (formula-ref-point ap)]
    [slp x c]))

(defn axis-intersect? [axis-pnt-1 axis-pnt-2 axis-int-pnt]
                 (if (or  
                       (and (<= axis-pnt-1 axis-pnt-2) (<= axis-pnt-1 axis-int-pnt) (>= axis-pnt-2 axis-int-pnt))
                       (and (>= axis-pnt-1 axis-pnt-2) (>= axis-pnt-1 axis-int-pnt) (<= axis-pnt-2 axis-int-pnt)))
                   true
                   false))

(defn find-intercept [ap1 ap2]
  (let [[slp1 x1 c1] (slp-int-formula ap1)
        [slp2 x2 c2] (slp-int-formula ap2)]   
  (cond 
      (or (= slp1 "fail") (= slp2 "fail"))
      "none"
      (and (= slp1 "inf") (= slp2 "inf"))
      (if (not= x1 x2) "none" "par")
      (= slp1 slp2)
       (is-boxed ap1 ap2)
      (= slp1 "inf") 
       [x1 (+ (* slp2 x1) c2)]
      (= slp2 "inf")  
       [x2 (+ (* slp1 x2) c1)]
      :defualt         
      (let [x (/ (- c2 c1) (- slp1 slp2))]
       [x (+ (* slp2 x) c2) c2 slp2 x]))))

(defn line-intersect? [start-x end-x start-y end-y x-int y-int]
  (and (axis-intersect? start-x end-x x-int)
       (axis-intersect? start-y end-y y-int)))

(defn is-intersect [path-1 path-2]
  (let [max-width (image/image-width (shared/image-ref))
        max-height (image/image-height (shared/image-ref))
        [start-x-1 start-y-1] (ant/ant-start-point path-1)
        [end-x-1 end-y-1] (ant/ant-end-point path-1)
        [start-x-2 start-y-2] (ant/ant-start-point path-2)
        [end-x-2 end-y-2] (ant/ant-end-point path-2)
        intercept (find-intercept path-1 path-2)
        [x-int y-int] intercept]
    (cond 
      (= "none" intercept)
      false
      (= "par" intercept)
      false
      ;(is-boxed path-1 path-2)
      (and (line-intersect? start-x-1 end-x-1 start-y-1 end-y-1 x-int y-int)
           (line-intersect? start-x-2 end-x-2 start-y-2 end-y-2 x-int y-int))
      true
      :default     
      false)))         

(defn make-group [non-group]
  (loop [to-group [(first non-group)] grouping [(first non-group)] working-non-group (rest non-group)]
    (if (empty? to-group)
      [grouping  working-non-group]
      (let [filter-group (filter  #(is-boxed(first to-group) %) working-non-group)]
        (if (not (empty? filter-group))
          (recur (apply conj  (rest to-group) filter-group )
                 (apply conj grouping filter-group)
                 (filter  #(not (is-boxed (first to-group) %)) working-non-group))
          (recur (rest to-group)
                 grouping
                 working-non-group))))))

(defn make-groups [cann-path]
  "Creates groups based on paths crossing"
  (loop [non-group cann-path groups [] meta-data {} group-id 0]
    (if (empty? non-group)
      {:groups groups :group-data meta-data}
      (let [[group-res non-group-res ] (make-group non-group)]
        (recur non-group-res  
               (apply conj groups (map #(assoc % :group group-id) group-res))
               (assoc meta-data (keyword (str group-id)) (count group-res))
               (inc group-id))))))

(defn salient-regions []
  "paths by salient region with minimal crossings"
  (let [groups (make-groups 
                 (shared/eval-paths))
        min-conn-group-ids (apply hash-map (flatten (filter #(>= (second %)( shared/min-conn-thresh)) (:group-data groups))))]
    (do (shared/salient-results min-conn-group-ids)
      (shared/salient-ids  (filter #(get (apply hash-map (flatten (shared/salient-results))) (keyword (str (:group %)))) (:groups groups))))))

(defn get-att-point-avg [att-key]
  (int (/ (apply + 
                 (map #(image/pix-value (att-key %) (shared/image-ref)) (shared/canidates))) 
          (count (shared/canidates)))))
 
(defn min-pxl-avg-can []
  (get-att-point-avg :local-min))

(defn max-pxl-avg-can []
  (get-att-point-avg :local-max))

(defn convex-hull [paths]
  (let[points (map #(Point. (first %) (second %))(map ant/ant-end-point paths))
       cnvx (ConvexHull. (into-array tgaa.hull.Point points))
       cnvx-points (. cnvx getConvexHull)]
    (map #(vector %1 %2)
         (map #(int (. % getX)) cnvx-points)
         (map  #(int (. % getY)) cnvx-points))))


(defn edge-stats-by-group[]
  (map #(vector (image/pix-value (:local-min %) (shared/image-gry-ref)) (:group %)) (shared/salient-ids)))
