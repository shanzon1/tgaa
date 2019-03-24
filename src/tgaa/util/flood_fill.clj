(ns tgaa.util.flood-fill)


(defn get-nb-pnts [x-ref y-ref thresh image]
  (filter #(not (or (nil? (last %))
                    (> (last %) thresh)
                    (=  (last %) 1))) 
          (map #(vector (first %) (second %) (safe-rgb (first %) (second %) image))
               [[(dec x-ref) y-ref] [(inc x-ref) y-ref] [x-ref (dec y-ref)] [x-ref (inc y-ref)]])))

(defn flood-loop [ x-in y-in thresh image ]
  (loop [pnts [[x-in y-in]]]
    (if (empty? pnts) 
       image
       (let [[x y] (first pnts)
             _ (. image setRGB x y 2)
             _ (println x " " y (. image getRGB x y))]
             (recur (concat (rest pnts) (get-nb-pnts x y thresh image)))))))

(defn flood 
  ([x y thresh graphic image]
    (if (and (>= x 1) (>= y 1)
             (< x (. image getWidth)) (< y (. image getHeight))
             (< thresh (. image getRGB x y)))
    (do (. image setRGB x y 100)
      (flood x (- y 1) thresh graphic image)
      (flood x (+ y 1) thresh graphic image)
      (flood (- x 1) y thresh graphic image)
      (flood (+ x 1) y thresh graphic image))
    [image graphic]))
  ([x y thresh image]
    (let [cpy-image (mikera.image.core/copy image)]
      (flood x y thresh (. cpy-image createGraphics) cpy-image))))


