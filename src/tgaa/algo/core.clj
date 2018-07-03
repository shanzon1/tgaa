(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.util.image :as image]
            [tgaa.util.ant-path :as ap]
            [tgaa.util.shared :as shared]))

(defn bootstrap[image]
  (apply min (ap/trial-min-local 
               (ap/proc-all-ants (ap/init-trail-paths image) image) image)))

(defn trap-ants [ant-paths image]
  (tgaa.util.ant-path/trail-max-of-min (tgaa.util.ant-path/escaped-ants ant-paths) image))

(defn trapping [image]
  (print "Not Implemented"))


(defn process-image [] 
  (let [image ^BufferedImage (image/image-RGB-gray (image/get-image))
        _    (shared/update-thresh (bootstrap image))]
             (shared/thresh)))
        