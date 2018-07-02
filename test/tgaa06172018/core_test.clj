(ns tgaa06172018.core-test
  (:require [clojure.test :refer :all]
            [tgaa06172018.core :refer :all]))

(let [ i (tgaa.util.image/get-image)]
  (proc-ant 
    {:start [136 121] :dir [1 -1]} i))