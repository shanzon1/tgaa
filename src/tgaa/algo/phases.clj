(ns tgaa.algo.phases
    (:require [tgaa.algo.ant-path :as ap]
              [tgaa.struct.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.struct.image :as image]
              [tgaa.util.filters :as filter]
              [tgaa.algo.analysis :as analysis]
              [tgaa.struct.ant :as ant]))

(defn load-image []
  (do (shared/save-image-ref (image/get-image))
    (shared/save-image-gry-ref ((filter/grayscale)(shared/image-ref)))))

(defn bootstrap[]
  (shared/update-thresh 
    (apply max (trial/trial-max-local 
                 (filter #(not= :dead (tgaa.struct.ant/ant-status %))
                         (ap/proc-all-ants (ap/init-trail-paths)))))))

(defn trapping-trial []
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
  (do (repeatedly 
        (shared/get-num-trails) #(trapping-trial))))

(defn evaluation [] 
  (do (shared/eval-paths (ap/reprocess-paths (shared/canidates)))
    (analysis/salient-regions)))

(defn evaluation-1 [] 
 (shared/eval-paths 
   (ap/reprocess-paths (shared/canidates))))

(defn evaluation-3 [] 
  (let [img ((tgaa.util.filters/edge) ((tgaa.util.filters/threshold (int (/ (shared/thresh) 2))) (shared/image-ref)))]
    img))

(defn analysis-hull[]
  "returns set of points representing the hull"
  (doall (shared/hull (analysis/convex-hull (shared/canidates (shared/get-num-trails)))) nil))
;;; not exicuting!!!!

