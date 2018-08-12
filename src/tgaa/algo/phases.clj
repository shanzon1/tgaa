(ns tgaa.algo.phases
    (:require [tgaa.util.ant-path :as ap]
              [tgaa.util.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.util.image :as image]
              [tgaa.java.util.filters :as filter]))

(defn load-image []
  (do (shared/save-image-ref (image/get-image))
    (shared/save-image-gry-ref ((filter/grayscale)(shared/image-ref)))))

(defn bootstrap[]
  (shared/update-thresh 
    (apply max(trial/trial-max-local 
               (ap/proc-all-ants (ap/init-trail-paths))))))

(defn perform-trial []
  (let [_ (shared/inc-trial)
        trial-paths (ap/proc-all-ants 
                      (ap/init-trail-paths))
        _ (shared/trial-info-path-cnt (count trial-paths))
        ta (trial/trapped-ants trial-paths)
        _ (shared/trial-info-cand (count ta))
        _ (shared/add-canidates ta)
        trial-thresh (trial/trap-escaped-thresh trial-paths)
        _ (shared/trial-info-thresh trial-thresh)
        _ (when (not (nil? trial-thresh)) (> trial-thresh (shared/thresh))
            (shared/update-thresh trial-thresh))]))

(defn trapping []
  (repeatedly (shared/get-num-trails) #(perform-trial)))

(defn evaluation [] 
  (trial/salient-regions))

