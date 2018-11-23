(ns tgaa.struct.ant)

(def status-trapped :trapped)
(def status-escaped :escaped)
(def status-dead :dead)
(def status-forage :forage)
(def dir-opt [[0 1][0 -1][1 0][-1 0][1 1][-1 -1][1 -1][-1 1]])

(defn create-ant-path[]
  "Creates init ant path datastruct"
  {:id (gensym)})

(defn ant-id [ant]
  (:id ant))

(defn attribute-set-get [key-name & in]
    (if (= (count in) 1)
      (key-name (first in))
      (assoc (second in) key-name (first in))))

(defn ant-group
  ([ant-path]
    (attribute-set-get :group ant-path))
  ([value ant-path]
    (attribute-set-get :group value ant-path)))

(defn ant-thresh? 
  ([ant-path]
    (attribute-set-get :thresh? ant-path))
  ([value ant-path]
    (attribute-set-get :thresh? value ant-path)))

(defn ant-trial-num
  ([ant-path]
    (attribute-set-get :trial-num ant-path))
  ([value ant-path]
    (attribute-set-get :trial-num value ant-path)))

(defn ant-status
  ([ant-path]
    (attribute-set-get :ant-status ant-path))
  ([value ant-path]
    (attribute-set-get :ant-status value ant-path)))

(defn ant-end-point
  ([ant-path]
    (attribute-set-get :end ant-path))
  ([value ant-path]
    (attribute-set-get :end value ant-path)))

(defn ant-start-point
  ([ant-path]
    (attribute-set-get :start ant-path))
  ([value ant-path]
    (attribute-set-get :start value ant-path)))

(defn ant-dir-vec
    ([ant-path]
    (attribute-set-get :dir ant-path))
  ([value ant-path]
    (attribute-set-get :dir value ant-path)))


(defn ant-local-min
  ([ant-path]
    (attribute-set-get :local-min ant-path))
  ([value ant-path]
    (attribute-set-get :local-min value ant-path)))

(defn ant-local-max
  ([ant-path]
    (attribute-set-get :local-max ant-path))
  ([value ant-path]
    (attribute-set-get :local-max value ant-path)))

(defn ant-path-length 
  ([ant-path]
    (attribute-set-get :path-length ant-path))
  ([value ant-path]
    (attribute-set-get :path-length value ant-path)))

(defn path-loc-at-time [ant-path time]
  [(+ (first (:start ant-path)) (* (first (:dir ant-path)) time))
  (+ (second (:start ant-path)) (* (second (:dir ant-path)) time))])
  
