(ns tgaa.util.image-band-filter)

(defn get-rgb-vec [image x y]
  (let [pxl (mikera.image.core/get-pixel image x y)]
  [(mikera.image.colours/extract-green pxl)
           (mikera.image.colours/extract-red pxl)
           (mikera.image.colours/extract-blue pxl)]))

(defn pxl-yellow [rgb-vec]
  (if (and (> (first rgb-vec) 200)
           (>  (second rgb-vec) 200)
           (< (nth rgb-vec 2) 100))
    true
    false))

(defn compare-color [ref]
   (doall (for [x (range (tgaa.struct.image/image-width ref))
          y (range (tgaa.struct.image/image-height ref))
              :when (not (pxl-yellow (get-rgb-vec ref x y)))]
     (. ref setRGB x y -1))))

;(def t2 (compare-color (shared/image-ref)))

(defn image-to-binary-yw [full-path target-folder]
  (let [ file-in-name (last (clojure.string/split full-path  #"\\"))
        file-in-parsed (clojure.string/split file-in-name #"\.")
        file-out (str target-folder "\\" (first file-in-parsed) "_YO." "png")
        conv-image (tgaa.struct.image/get-image full-path)
        _ (compare-color conv-image)
        _ (mikera.image.core/save conv-image  file-out)]))

(defn convert-yellow-only []
  (let [conv-files (tgaa.util.gui/sys-folder-all-files "Parent folder to convert all images.")
        target-folder (str (tgaa.util.gui/sys-view-ref "Target parent folder." :folder (str (first conv-files)) ))]
        (loop [file (rest conv-files)]
          (if (empty? file)
            (println "complete")
            (do (image-to-binary-yw (str (first file)) target-folder)
              (recur (rest file)))))))

(defn convert-yellow-image []
  (let [conv-file (str (tgaa.util.gui/sys-view-ref "Parent folder to convert all images." :file))
        helper-path (apply str  (interpose "\\" (butlast (clojure.string/split conv-file #"\\" ))))
        target-folder (str (tgaa.util.gui/sys-view-ref "Target parent folder." :folder helper-path))]
             (image-to-binary-yw conv-file target-folder)))
