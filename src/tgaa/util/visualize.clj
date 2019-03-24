(ns tgaa.util.visualize
  (:require [tgaa.struct.shared :as shared]
            [clojure.test :refer [is]]
            [tgaa.util.filters :as filter]
            [clojure.string :refer [upper-case]]
            [tgaa.struct.ant :as ant]
            [mikera.image.core :as mi]
            [tgaa.struct.image :as image])
  (:import [javax.imageio ImageIO]
            [java.io File]
            [java.awt.image BufferedImage]
            [java.awt BasicStroke]
            [java.awt Color]))
(def line-size 3)


(defn show-image []
  (mi/show (shared/image-ref)))

(defn draw-ant-path [{:keys [start end]} graphic]
  (. graphic drawLine (first start) (second start) (first end) (second end)))

(defn draw-ant-point [point graphic draw-weight]
  (let [offset (Math/round (/ draw-weight 2.0))]
  (. graphic fillOval (- (first point) offset) (- (second point) offset)  draw-weight draw-weight)))

(defn draw-header [string img-ref]
    (let [g (image/build-graphic 1 "YELLOW" img-ref)]
    (doall (image/draw-string string g 10 10))
    img-ref))

(defn draw-ant-end-pnts 
  ([ant-paths img-ref line-size header]
    (let [i (mi/copy img-ref)
          g (image/build-graphic line-size "YELLOW" i)
          _  (doall (map #(draw-ant-point (ant/ant-end-point %) g line-size) ant-paths))
          _ (draw-header "Ant end points." i)]   
      i))
  ([ant-paths img-ref line-size]
    (let [i (mi/copy img-ref)
          g (image/build-graphic line-size "YELLOW" i)
          _  (doall (map #(draw-ant-point (ant/ant-end-point %) g line-size) ant-paths))]   
      i)))

(defn draw-ant-start-pnts [ant-paths img-ref line-size]
  (let [i (mi/copy img-ref)
        g (image/build-graphic 1 "BLUE" i)
        _  (doall (map #(draw-ant-point (ant/ant-start-point %) g line-size) ant-paths))]
  i))

(defn draw-paths [ant-paths color-name-str img-ref line-width]
  (let [g (image/build-graphic line-width color-name-str img-ref)]
    (doall (map 
             #(draw-ant-path % g) ant-paths))
    img-ref))

(defn filtered-draw-paths [ant-paths-filter-func color-name-str img-ref line-width]
    (draw-paths (filter ant-paths-filter-func (shared/canidates)) color-name-str img-ref line-width))

(defn draw-eval-paths 
  ([]
  (let [i (mi/copy (shared/image-ref))
    _ (draw-paths (shared/eval-paths) "YELLOW" i 2)]
   i ))
    ([header]
  (let [i (mi/copy (shared/image-ref))
    _ (draw-paths (shared/eval-paths) "YELLOW" i 2)
    _ (draw-header (str header) i)]
   i )))

(defn show-val-end-pnts []
  (mi/show (draw-ant-end-pnts (shared/eval-paths) (shared/image-gry-ref) 2 "Ant end points.")))

(defn show-prediction-pnts []
  (mi/show (draw-ant-end-pnts (shared/eval-paths) (shared/image-gry-ref) 8)))

(defn show-val-start-pnts []
  (mi/show (draw-ant-start-pnts (shared/eval-paths) (shared/image-gry-ref) 1)))

(defn show-val-boundry-pnts []
  (mi/show (draw-ant-start-pnts
             (shared/eval-paths) 
             (draw-ant-end-pnts (shared/eval-paths) (shared/image-gry-ref) 1 "Ant end points."))))

(defn show-all-boundry-pnts []
  "Displays start and end points for all cannidate paths"
  (mi/show (draw-ant-start-pnts
             (shared/canidates) 
             (draw-ant-end-pnts (shared/canidates) (shared/image-gry-ref) 3)3)))



(defn show-all-boundry-pnts-cann []
  "Displays start and end points for all cannidate paths"
  (mi/show (draw-ant-start-pnts (shared/canidates) 
                                (draw-ant-end-pnts (shared/canidates) 
                                                   (draw-paths (shared/canidates) "GREEN" (shared/image-gry-ref) 1) 1))))

(defn show-eval-paths[]
  (mi/show (draw-eval-paths "Pruned paths.")))

(defn show-eval-paths-result[]
  (mi/show (draw-eval-paths)))

(defn draw-can-paths
  ([]
    (let [i (mi/copy (shared/image-ref))
          _ (filtered-draw-paths (fn [x] true) "YELLOW" i 2)
          _  (draw-header (str "All candidates.") i)]
      i))
  ([trial-num]
    (let [i (mi/copy (shared/image-ref))
          _ (filtered-draw-paths (fn [x] (= (ant/ant-trial-num x) trial-num)) "YELLOW" i 2)
          _  (draw-header (str "Trapping Trial Number: " trial-num) i)]
      i)))

(defn show-segmentaton 
   ([theshold]
    (mi/show ((filter/threshold theshold) (shared/image-ref))))
  ([]
  (show-segmentaton (/ (shared/thresh) 2))))


(defn show-cann-path
  ([]
    "show all trap paths"
    (mi/show (draw-can-paths)))
  ([trial-num] 
    "show trap path for i th trial"
    (mi/show (draw-can-paths trial-num))))

(defn anim-trail-paths[]
  (loop [i 0]
    (if (= i (shared/get-num-trails))
      nil
      (do (Thread/sleep 1000) (show-cann-path i) (recur (inc i))))))
  
(defn draw-boundary [ref-img boundary]
  (let [boundary-closed (concat boundary [(first boundary)])
        xpnts (doall (map first  boundary-closed))
        ypnts (doall (map second boundary-closed))
        p-num (count boundary-closed)
        g (. ref-img createGraphics)
        _ (doto g (.setColor (. Color YELLOW))
            (.setStroke (java.awt.BasicStroke. 4))
            (.drawPolyline (int-array p-num xpnts) (int-array p-num ypnts) p-num))]
    ref-img))

(defn draw-final-boundary[]
  (mikera.image.core/show
    (draw-boundary (mikera.image.core/copy (shared/image-ref))
                   (shared/hull))))

(defn draw-ant-paths[]
  (mikera.image.core/show
    (draw-boundary (mikera.image.core/copy (shared/image-ref))
                   (shared/hull))))

(defn show-salient-paths
  ([]
    (mikera.image.core/show
      (draw-paths (shared/salient-ids) "YELLOW" (mikera.image.core/copy (shared/image-ref)) 2)))
  ([group-num] (mikera.image.core/show
             (draw-paths (filter #(= (:group %) group-num) (shared/salient-ids)) "YELLOW" (mikera.image.core/copy (shared/image-ref)) 2))))

(defn show-image []
  (mi/show (shared/image-ref)))

(defn animate-algo []
  (do (anim-trail-paths)
    (Thread/sleep 1000) 
    (show-cann-path)
    (Thread/sleep 2000) 
    (show-eval-paths)
    (Thread/sleep 2000) 
    (show-all-boundry-pnts)))