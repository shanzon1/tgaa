(ns tgaa.util.shared)

(def session (atom {:trial-num 0
                    :image-location "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"
                    :cand-paths []
                    :thresh 0}))

(defn get-config "Gets Configuration set in text file" [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(defn inc-trial[]
  "Increments session map :trial-num"
  (reset! session 
          (assoc @session :trial-num 
                 (inc 
                   (:trial-num @session)))))


(def config (get-config))

(defn update-thresh [thresh]
  (reset! session (assoc @session :thresh thresh)))

(defn trial-num []
  (:trial-num @session ))

(defn thresh []
  (:thresh @session))