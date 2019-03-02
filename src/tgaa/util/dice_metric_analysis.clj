(ns tgaa.util.dice-metric-analysis
      (:require [tgaa.algo.ant-path :as ap]
              [tgaa.struct.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.struct.image :as image]
              [tgaa.util.filters :as filter]
              [tgaa.algo.analysis :as analysis]
              [tgaa.util.gui :as gui]))

(defn get-sample-image []
  (image/get-image 
    (str (gui/sys-view-ref "Select sample image (manual)" :file))))


(defn get-reference-image []
  (image/get-image 
    (str (gui/sys-view-ref "Select reference image (automation)" :file))))

(defn compare-pix [x y sample-one sample-two ref]
  (let[sample-one-val (. sample-one getRGB x y)
       sample-two-val (. sample-two getRGB x y)
       ref-val (. ref getRGB x y)]
  (cond 
    (and (not= ref-val sample-one-val)
         (not= ref-val sample-two-val))
    :intersect
    (not= ref-val sample-one-val)
    :sample-one
    (not= ref-val sample-two-val)
    :sample-two
    :default
    :none
    )))

(defn compare-color [sample-one sample-two ref]
  (let [width-ref (image/image-width ref)
        height-ref (image/image-height ref)
        width-one (image/image-width sample-one)
        height-one (image/image-height sample-one)
        width-two (image/image-width sample-two)
        height-two (image/image-height sample-two)
        ]
  (if (or (not= width-one  width-ref) 
          (not= height-one height-ref)
          (not= width-two  width-ref) 
          (not= height-two height-ref))
    (Exception. 
      (str "Images must be same size"
           "ref-image: "  height-ref " " width-ref "\n"
           "sample-one: "  height-one " " width-one "\n"
           "sample-two: "  height-two " " width-two "\n"))
    (for [x (range width-ref)
          y (range height-ref)]
      (compare-pix x y sample-one sample-two ref)))))
      
      
(defn perform-dice []
  (let [default-dir (str (gui/sys-view-ref "Select reference image (uneditted)" :file))
        reference-image (image/get-image default-dir)
        default-dir (str (gui/sys-view-ref "Select sample two image" :file default-dir))
        sample-one (image/get-image default-dir)
        default-dir (str (gui/sys-view-ref "Select sample one image" :file default-dir))
        sample-two (image/get-image default-dir)]
       (compare-color sample-one sample-two reference-image)))

(defn dice-metric [ pix-res]
  (let [dice-numurator (* 2 (count (filter #(= :intersect %)   pix-res)))
        diff (count (filter #(or (= :sample-one %) (= :sample-two %)) pix-res))
        dice-denominator (+ dice-numurator  diff )]
    (if (or (nil? dice-denominator ) (= 0 dice-denominator))
      0
      {:dice (double (/ dice-numurator  dice-denominator))
       :diff-total-pred (count (filter #(or (= :sample-one %) (= :sample-two %)) pix-res))
       :sample-one-pred (count (filter #(= :sample-one %) pix-res))
       :sample-two-pred (count (filter #(= :sample-two %) pix-res))
       :no-diff (count (filter #(= :none %) pix-res))
       :intersect(count (filter #(= :intersect %) pix-res))
       })))
