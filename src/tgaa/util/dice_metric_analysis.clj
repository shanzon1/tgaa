(ns tgaa.util.dice-metric-analysis
      (:require [tgaa.algo.ant-path :as ap]
              [tgaa.struct.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.struct.image :as image]
              [tgaa.util.filters :as filter]
              [tgaa.algo.analysis :as analysis]
              [tgaa.util.gui :as gui]))

(def black-max -16480096)

(def quick-path  "C:\\Users\\erudi\\Desktop\\Thesis End Game\\Sampling\\Student Sampling\\")

(defn file-to-path-only [path]
  (apply str  (interpose "\\" (butlast (clojure.string/split path #"\\" )))))

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
        sample-loc-1 (str (gui/sys-view-ref "Select sample image one" :file quick-path))
        _ (println sample-loc-1)
        sample-1 (image/get-image sample-loc-1)
        sample-loc-2 (str (gui/sys-view-ref "Select sample image two" :file (file-to-path-only sample-loc-1)))
        _ (println sample-loc-2)
        sample-2 (image/get-image sample-loc-2)
        sample-loc-3 (str (gui/sys-view-ref "Select sample image three" :file (file-to-path-only sample-loc-2)))
        _ (println sample-loc-3)
        sample-3 (image/get-image sample-loc-3)
        predicted-loc (str (gui/sys-view-ref "Select predicted image" :file (file-to-path-only sample-loc-3)))
        _ (println predicted-loc "\n\n\n")
        predicted (image/get-image predicted-loc)]
       [{:results (compare-color  predicted sample-1 )
         :sample-loc sample-loc-1
         :predicted-loc predicted-loc
         :type :pred}
        
        {:results (compare-color  predicted sample-2 )
         :sample-loc sample-loc-2
         :predicted-loc predicted-loc
         :type :pred}
        
        {:results (compare-color  predicted sample-3 )
         :sample-loc sample-loc-3
         :predicted-loc predicted-loc
         :type :pred}
        
        {:results (compare-color  sample-1 sample-2 )
         :sample-loc sample-loc-1
         :predicted-loc sample-loc-2
         :type :sample}
        
        {:results (compare-color  sample-2 sample-3 )
         :sample-loc sample-loc-2
         :predicted-loc sample-loc-3
         :type :sample}
        
        {:results (compare-color  sample-1 sample-3 )
         :sample-loc sample-loc-1
         :predicted-loc sample-loc-3
         :type :sample}
        ]))

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
  (map #(compute-dice-metric %)   (perform-dice-analysis)))