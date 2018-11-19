(ns tgaa.algo.phases
    (:require [tgaa.algo.ant-path :as ap]
              [tgaa.struct.shared :as shared]
              [tgaa.algo.trial :as trial]
              [tgaa.struct.image :as image]
              [tgaa.util.filters :as filter]
              [tgaa.algo.analysis :as analysis]))

(defn load-image []
  (do (shared/save-image-ref (image/get-image))
    (shared/save-image-gry-ref ((filter/grayscale)(shared/image-ref)))))

(defn bootstrap[]
  (shared/update-thresh 
    (apply max(trial/trial-max-local 
               (ap/proc-all-ants (ap/init-trail-paths))))))

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



(defn reprocess-paths []
  "evaluation phase: prune paths that threshold omits and reprocess all paths with final threshold"
  (shared/eval-paths
    (filter #(> (tgaa.struct.ant/ant-path-length %) (shared/min-path-len))
            (map 
              tgaa.algo.ant-path/proc-ant  
              (filter #(>
                         (shared/thresh) (image/pix-value (tgaa.struct.ant/ant-local-min %) (shared/image-gry-ref)))
                      (shared/canidates))))))

(defn trapping []
  (do (repeatedly (shared/get-num-trails) #(trapping-trial))))

(defn evaluation [] 
  (do (reprocess-paths)
    (analysis/salient-regions)) nil)

(defn analysis-hull[]
  "returns set of points representing the hull"
  (shared/hull (analysis/convex-hull (shared/canidates (shared/get-num-trails)))))
;;; not exicuting!!!!

