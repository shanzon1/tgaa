(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.algo.phases :as phase]))



(defn process-image [] 
  (do(phase/load-image)
       (phase/bootstrap)
       (phase/trapping)))
        