(ns tgaa06172018.analysis)

(let [ i (get-image)
       c 500
       
      rx  (repeatedly c #(rand-int (dec (. i getWidth))))
      ry (repeatedly c #(rand-int (dec (. i getHeight))))]
  (time
    (loop [v (dec c) px 0]
      (if (< v 1)
        "done"
        (recur (dec v) (/ px 0(. i getRGB (nth rx v) (nth ry v))))))))


(let [i (get-image)
     c 5000
      rx  (repeatedly c #(rand-int (dec (. i getWidth))))
      ry (repeatedly c #(rand-int (dec (. i getHeight))))
     vi (let [x-w (dec (. i getWidth))
              y-h (dec (. i getHeight))]
          (loop [x-cnt 0 res []]
            (if (>= x-cnt x-w)
              res
              (recur (inc x-cnt)
                     (conj res
                           (loop [y-cnt 0 res-c []]
                             (if (>= y-cnt y-h)
                               res-c
                               (recur (inc y-cnt) (conj res-c (. i getRGB x-cnt y-cnt))))))))))]
  (time
    (loop [v (dec c) p []]
     (if (< v 1)
      "done"
      (recur (dec v) (conj p (nth (nth vi (nth rx v )) (nth ry v))))))))


(let [i (get-image)
     c 5000
      rx  (repeatedly c #(rand-int (dec (. i getWidth))))
      ry (repeatedly c #(rand-int (dec (. i getHeight))))
     vi (let [x-w (dec (. i getWidth))
              y-h (dec (. i getHeight))]
          (loop [x-cnt 0 res {}]
            (if (>= x-cnt x-w)
              res
              (recur (inc x-cnt)
                     (merge res
                           (loop [y-cnt 0 res-c {}]
                             (if (>= y-cnt y-h)
                               res-c
                               (recur (inc y-cnt) (assoc res-c  (keyword (str x-cnt "-" y-cnt))(. i getRGB x-cnt y-cnt))))))))))]
  (time
    (loop [v (dec c) p []]
     (if (< v 1)
      "done"
      (recur (dec v) (conj p ((keyword (str (nth rx v) "-" (nth ry v))) vi)))))))