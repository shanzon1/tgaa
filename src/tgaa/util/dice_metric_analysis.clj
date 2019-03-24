(ns tgaa.util.dice-metric-analysis
      (:require [tgaa.algo.ant-path :as ap]
              [tgaa.struct.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.struct.image :as image]
              [tgaa.util.filters :as filter]
              [tgaa.algo.analysis :as analysis]
              [tgaa.util.gui :as gui]))

(def black-max -16480096)

(defn get-sample-image []
  (image/get-image 
    (str (gui/sys-view-ref "Select sample image (manual)" :file))))


(defn get-reference-image []
  (image/get-image 
    (str (gui/sys-view-ref "Select reference image (automation)" :file))))

(defn compare-pix [x y predicted sample]
  (let [sample-val (. sample getRGB x y)
        predicted-val (. predicted getRGB x y)]
  (cond 
    (and (not= -1 sample-val)
         (not= -1 predicted-val))
    :intersect
    (not= -1 sample-val)
    :sample
    (not= -1 predicted-val)
    :predicted
    :default
    :none
    )))

(defn compare-color [sample-one sample-two]
  (let [
        width-one (image/image-width sample-one)
        height-one (image/image-height sample-one)
        width-two (image/image-width sample-two)
        height-two (image/image-height sample-two)
        ]
  (if (or (not= width-one  width-two) 
          (not= height-one height-two))
    (Exception. 
      (str "Images must be same size"
           "sample-one: "  height-one " " width-one "\n"
           "sample-two: "  height-two " " width-two "\n"))
    (for [x (range width-one)
          y (range height-one)]
      (compare-pix x y sample-one sample-two)))))
      
      
(defn perform-dice-analysis []
  (let [
        sample-loc (str (gui/sys-view-ref "Select sample image" :file ))
        sample (image/get-image sample-loc)
        predicted-loc (str (gui/sys-view-ref "Select predicted image" :file sample-loc))
        predicted (image/get-image predicted-loc)]
       {:results (compare-color  predicted sample )
        :sample-loc sample-loc
        :predicted-loc predicted-loc}))

(defn compute-dice-metric [dm-data]
  (let [pix-res (:results  dm-data)
        dice-numurator (* 2 (count (filter #(= :intersect %)   pix-res)))
        diff (count (filter #(or (= :sample %) (= :predicted %)) pix-res))
        dice-denominator (+ dice-numurator  diff )]
    (if (or (nil? dice-denominator ) (= 0 dice-denominator))
      0
     (merge 
       {:dice (double (/ dice-numurator  dice-denominator))
        :diff-total-pred (count (filter #(or (= :sample %) (= :predicted %)) pix-res))
        :sample-pnts-missed (count (filter #(= :sample %) pix-res))
        :predicted-pnts-incorrect (count (filter #(= :predicted %) pix-res))
        :no-diff-pnts (count (filter #(= :none %) pix-res))
        :predicted-pnts-correctly (count (filter #(= :intersect %) pix-res))}
        (dissoc dm-data :results)))))

(defn dice-metric []
  (compute-dice-metric   (perform-dice-analysis)))